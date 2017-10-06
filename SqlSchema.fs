//------------------------------------------------------------
// sqlschema.fs       Exports native records into SQL
//
// 2007 written by Ralf Herbrich
//
// Copyright (c) 2002-2011 Microsoft Research Ltd.
//
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
//
//------------------------------------------------------------

namespace MSRC.Tools.Sql

module SqlSchema = 

    //------------------------------------------------------------------------
    // Types for the SQL schema library
    //------------------------------------------------------------------------

    open System

    /// Attribute to indicate the length of a string field
    [<AttributeUsageAttribute(AttributeTargets.Field)>]
    type SqlStringLengthAttribute(length:int) =
        inherit Attribute() 
        /// Maximal length of a string
        member __.Length  =length

    open System.Text
    open System.Reflection    
    open System.Data
    open System.Collections.Generic
    open System.Data.SqlClient
    open System.Data.SqlTypes
    open Microsoft.FSharp.Reflection

    /// A Sql command schema is an interface for a type to Sql data store commands
    type ISqlCommandSchema<'a> =
        /// Returns the CREATE TABLE statements for the type
        abstract Create : unit -> string
        /// Returns the DROP TABLE statements for the type
        abstract Drop : unit -> string
        /// Returns the INSERT statements for an instance of the type
        abstract Insert : 'a -> string

    /// A DataSet command schema is an interface for a type to ADO DataSet data store 
    type IDataSetSchema<'a> =
        /// Returns the DataSet that represents the data schema
        abstract Clear : unit -> unit
        /// Returns the insert command into the dataset for an instance of the type
        abstract Insert : 'a -> unit
        /// Returns the current dataset
        abstract Dataset : DataSet

    /// A Sql command schema is an interface for a type to a Sql database
    type ISqlSchema<'a> =
        /// Create the schema in the database
        abstract Create : unit -> unit
        /// Drop the schema in the database
        abstract Drop : unit -> unit
        /// Inserts an item into the database
        abstract Insert : 'a -> unit

    /// A Sql command schema is an interface for a type to a Sql database
    type IBulkInsertSqlSchema<'a> =
        /// Inserts an item into the database
        abstract Insert : 'a -> unit
        /// Pushes the current write buffer to the Sql database (via bulkcopy)
        abstract Flush : unit -> unit

    /// The info of a SQL base type
    type SqlBaseTypeInfo =
        {
            /// The .Net type of the SQL base type
            DotNetType          : Type
            /// The default value of a DotNet Type
            DefaultDotNetValue  : obj
            /// Function that gets the value in memory format from an object
            DotNetValue         : obj -> obj
            /// Name of the type in SQL
            SqlTypeName         : string
            /// Function that gets the value in SQL format from an object
            SqlValue            : obj -> string
        }

    /// The internal representation of a SQL type            
    type SqlType =
        | SqlRecord     of (string * SqlType) list * (obj -> obj [])
        | SqlArray      of SqlType 
        | SqlOption     of SqlType * (obj -> obj)
        | SqlBaseType   of SqlBaseTypeInfo

    //------------------------------------------------------------------------
    // Type compiler from .NET type to a SQL type
    //------------------------------------------------------------------------

    /// Computes the SQL type from the .NET/F# type. Also returns a list of tables for all enumeration types encountered
    let DotNetTypeToSqlType ty =     
        // The global table that holds all dictionaries of the enum tables
        let enumTables = new Dictionary<_,SortedList<int,string>> ()    
        
        /// Type information of the Sql base types     
        let SqlBaseTypeInfo (t:Type) att =                    
            /// Calls the ToString method
            let ToString (x:obj) = x.ToString ()
            /// The ID method
            let Id (x:obj) = x
            /// Builds a string base info using custom attributes; adds an exception handler at runtime that checks that the string submitted is never longer
            let StringBaseInfo () = 
                let n = 
                    try
                        let ssla = att |> Seq.find (fun (o:obj) -> (o :? SqlStringLengthAttribute)) :?> SqlStringLengthAttribute
                        ssla.Length
                    with
                    | _ -> 255
                /// The Reader for SQL strings (with built-in length checks)
                let PrintSqlString (x:obj) = 
                    let s = x :?> string
                    if s.Length > n then
                        raise (new ArgumentException (sprintf "'%s' is too long. Expecting a %d characters long string" s n))
                    else
                        "'" + s.Replace("'","''") + "'"
                /// The "reader" for .Net strings (with built-in length checks)
                let GetString (x:obj) = 
                    match x with
                    | :? string as s when s.Length > n  -> raise (new ArgumentException (sprintf "'%s' is too long. Expecting a %d characters long string" s n))
                    | _                                 -> x
                { DotNetType = t; DefaultDotNetValue = (box String.Empty); DotNetValue = GetString; SqlTypeName=(sprintf "varchar(%d)" n); SqlValue = PrintSqlString }
                
            match t with
            | t when t = typeof<Guid>       -> { DotNetType = t;    DefaultDotNetValue = (box Guid.Empty);          DotNetValue = Id;   SqlTypeName="uniqueidentifier"; SqlValue = (fun x -> "'" + (ToString x) + "'") } 
            | t when t = typeof<Boolean>    -> { DotNetType = t;    DefaultDotNetValue = (box false);               DotNetValue = Id;   SqlTypeName="bit";              SqlValue = (fun x -> "'" + (ToString x) + "'") }
            | t when t = typeof<byte>       -> { DotNetType = t;    DefaultDotNetValue = (box 0uy);                 DotNetValue = Id;   SqlTypeName="tinyint";          SqlValue = ToString }
            | t when t = typeof<char>       -> { DotNetType = t;    DefaultDotNetValue = (box '?');                 DotNetValue = Id;   SqlTypeName="char";             SqlValue = (fun x -> "'" + (ToString x) + "'") }
            | t when t = typeof<DateTime>   -> { DotNetType = t;    DefaultDotNetValue = (box DateTime.MinValue);   DotNetValue = Id;   SqlTypeName="datetime";         SqlValue = (fun x -> "'" + (x :?> DateTime).ToString ("s") + "'") }
            | t when t = typeof<Decimal>    -> { DotNetType = t;    DefaultDotNetValue = (box Decimal.Zero);        DotNetValue = Id;   SqlTypeName="decimal";          SqlValue = ToString }
            | t when t = typeof<float>      -> { DotNetType = t;    DefaultDotNetValue = (box 0.0);                 DotNetValue = Id;   SqlTypeName="float";            SqlValue = ToString }
            | t when t = typeof<int16>      -> { DotNetType = t;    DefaultDotNetValue = (box 0s);                  DotNetValue = Id;   SqlTypeName="smallint";         SqlValue = ToString }
            | t when t = typeof<int32>      -> { DotNetType = t;    DefaultDotNetValue = (box 0);                   DotNetValue = Id;   SqlTypeName="int";              SqlValue = ToString }
            | t when t = typeof<int64>      -> { DotNetType = t;    DefaultDotNetValue = (box 0L);                  DotNetValue = Id;   SqlTypeName="bigint";           SqlValue = ToString }
            | t when t = typeof<sbyte>      -> { DotNetType = t;    DefaultDotNetValue = (box 0y);                  DotNetValue = Id;   SqlTypeName="tinyint";          SqlValue = ToString }
            | t when t = typeof<single>     -> { DotNetType = t;    DefaultDotNetValue = (box 0.0f);                DotNetValue = Id;   SqlTypeName="real";             SqlValue = ToString }
            | t when t = typeof<uint16>     -> { DotNetType = t;    DefaultDotNetValue = (box 0us);                 DotNetValue = Id;   SqlTypeName="smallint";         SqlValue = ToString }   
            | t when t = typeof<uint32>     -> { DotNetType = t;    DefaultDotNetValue = (box 0ul);                 DotNetValue = Id;   SqlTypeName="int";              SqlValue = ToString }
            | t when t = typeof<uint64>     -> { DotNetType = t;    DefaultDotNetValue = (box 0UL);                 DotNetValue = Id;   SqlTypeName="bigint";           SqlValue = ToString }
            | t when t = typeof<string>     -> StringBaseInfo ()
            | _                             -> failwith "Unknown base type"
            
        /// Converts a .Net system type into a Sql type
        let rec GetType (ty: Type) att =
            /// Checks if the name type list tl is that of an Enum type
            let CheckEnum (tl:Reflection.UnionCaseInfo[])  = tl |> Array.forall (fun uc -> uc.GetFields().Length = 0)
            /// Returns the Enum type (description)
            let EnumTypeInfo (tl:Reflection.UnionCaseInfo[]) = 
                /// Add all the possible constructor values to the global and local Enum table 
                tl |> Seq.iter (fun uc -> 
                    if not (enumTables.ContainsKey (ty.Name)) then enumTables.Add (ty.Name, new SortedList<_,_> ()) 
                    (enumTables.[ty.Name]).Add (uc.Tag, uc.Name)
                )
                /// Retrieves the ID of 
                let tagReader = FSharpValue.PreComputeUnionTagReader ty
                let enumIDReader = (fun obj -> string (tagReader obj))
                { DotNetType = typeof<int>; DefaultDotNetValue = (box 0); DotNetValue = (fun x -> (tagReader x) :> obj); SqlTypeName="int"; SqlValue = enumIDReader }
                
            if ty.IsArray then
                SqlArray (GetType (ty.GetElementType()) [||])
            elif ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<int option> then
                let cases = FSharpType.GetUnionCases ty
                let f = FSharpValue.PreComputeUnionReader cases.[1]
                SqlOption ((GetType (ty.GetGenericArguments()).[0] att), fun (x:obj) -> (f x).[0] ) 
            elif FSharpType.IsRecord ty then 
                SqlRecord ([ for p in FSharpType.GetRecordFields ty do
                                  let attr = p.GetCustomAttributes (true) 
                                  yield (p.Name, GetType ty attr) ], FSharpValue.PreComputeRecordReader ty)
            elif FSharpType.IsTuple ty then 
                SqlRecord ([ for (p,i) in FSharpType.GetRecordFields ty  |> Seq.mapi (fun i x -> (x,i)) do
                                 yield sprintf "Field%d" i, GetType ty [||] ], FSharpValue.PreComputeTupleReader ty)
            elif FSharpType.IsUnion ty && CheckEnum (FSharpType.GetUnionCases ty) then
                 SqlBaseType (EnumTypeInfo (FSharpType.GetUnionCases ty))
            else 
                SqlBaseType (SqlBaseTypeInfo ty att)
        
        /// Initiate the type finding on type 'a
        let sqlType = GetType ty [||]
        (sqlType, enumTables |> Seq.map (fun kvp -> (kvp.Key, kvp.Value)))

    /// Computes a type name for a given .Net/F# type
    let rec TypeName (ty:Type) = 
        if ty.IsArray then TypeName (ty.GetElementType())
        elif ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<int option> then TypeName ((ty.GetGenericArguments()).[0])
        elif FSharpType.IsTuple ty then "Tuple" 
        else ty.Name

    //------------------------------------------------------------------------
    // SQL Type to a SqlCommand schema interpreter
    //------------------------------------------------------------------------

    /// Builds a schema of SQL commands for creating and inserting into an Sql database
    let SqlTypeToISqlCommandSchema sqlType baseTableName enumTables = 
        /// Creates a SQL statement from a Sql type
        let (CreateStatement,DropStatement) = 
            /// The string builder the gets build into for the CREATE statment
            let createText = new StringBuilder ()
            /// A helper function to print strings into the CREATE string builder
            let Append (s:string) = createText.Append (s) |> ignore
            
            /// A helper function to recursively build the CREATE statement
            let rec CreateStatementHelper parentName name sqlType = 
                let foreignKeyText = if parentName = "" then "" else "[" + parentName + "Id] bigint, "
                
                /// Get the SQL base type information (if there is any) or None
                let GetSqlBaseTypeInfo (f,t) =
                    match t with 
                    | SqlOption (SqlBaseType bi,_)  -> Some ("[" + f + "] " + bi.SqlTypeName + " NULL")
                    | SqlBaseType bi                -> Some ("[" + f + "] " + bi.SqlTypeName + " NOT NULL")
                    | _                             -> None
                /// Writes the SQL base type information (if there is any) or None
                let WriteCreateStatementForNonBaseTypes (f,t) =
                    match t with 
                    | SqlOption (SqlBaseType bi,_)  -> Set.empty
                    | SqlBaseType bi                -> Set.empty
                    | _                             -> CreateStatementHelper name (name + f) t
                /// Checks if the fields of a record are all "basic", that is, they never spawn a new table creation in which case
                /// we do not need an Id column
                let SimpleRecord fts = 
                    fts |> List.forall (fun (_,ty) -> 
                        match ty with 
                        | SqlOption (SqlBaseType _,_)   -> true 
                        | SqlBaseType _                 -> true 
                        | _                             -> false
                    )                                    
                /// Main recursion
                match sqlType with 
                | SqlRecord (fts,reader)    -> 
                    Append "CREATE TABLE ["; Append name; Append "] ("; 
                    if not (SimpleRecord fts) then Append name; Append "Id bigint IDENTITY (1,1), "; 
                    Append foreignKeyText
                    Append ((fts |> List.choose GetSqlBaseTypeInfo |> String.concat ",") + ")\r\n")
                    fts |> List.fold (fun acc x -> Set.union acc (WriteCreateStatementForNonBaseTypes x)) (Set.singleton name)
                | SqlArray t                -> CreateStatementHelper parentName name t
                | SqlOption(t,_)            -> CreateStatementHelper parentName name t
                | SqlBaseType bi            -> 
                    Append "CREATE TABLE ["; Append name; Append "] ("; Append foreignKeyText; Append "[Value] "; Append bi.SqlTypeName; Append ")" 
                    Set.singleton name
                
            let tables = CreateStatementHelper "" baseTableName sqlType
            (createText.ToString (), tables |> Set.toList |> List.map (sprintf "DROP TABLE [%s]") |> String.concat "\r\n")
            

        /// The CREATE TABLE/INSERT INTO commands for the Enum Id tables
        let DropCreateInsertEnumStatements =
            // Generate the drop/create/insert statements
            enumTables |> Seq.map ( fun (name,fields) ->
                let dropCreateCommand = sprintf "DROP TABLE [%s]\r\nCREATE TABLE [%s] ([%sId] int, [Name] varchar(255))" name name name
                let insertCommand = 
                    fields 
                        |> Seq.map (fun (row:KeyValuePair<_,_>) -> 
                            sprintf "INSERT INTO [%s] ([%sId], [Name]) VALUES (%d, '%s')" name name row.Key row.Value) 
                        |> Seq.toList 
                        |> String.concat "\r\n"
                dropCreateCommand + "\r\n" + insertCommand
            )
            |> Seq.toList
            |> String.concat "\r\n"

        /// Computes the INSERT INTO statements
        let WriteInsertStatement (insertIntoText:StringBuilder) v = 
            /// A helper function to print strings into the INSERT INTO string builder
            let Append (s:string) = insertIntoText.Append (s) |> ignore

            /// The recursive function that computes the INSERT INTO statements
            let rec WriteInsertStatementHelper baseIdInfo name sqlType v = 
                /// Is there any SQL base type information?
                let IsSqlBaseValue f t =
                    match t with 
                    | SqlOption (SqlBaseType bi,reader) -> true
                    | SqlBaseType bi                    -> true
                    | _                                 -> false
                /// Get the SQL base type value (if there is any), else fail
                let GetSqlBaseValue t v =
                    match t with 
                    | SqlOption (SqlBaseType bi,reader) -> 
                        match v with 
                        | null  -> "NULL"
                        | _     ->  bi.SqlValue (reader v)
                    | SqlBaseType bi                    -> bi.SqlValue v
                    | _                                 -> failwith "GetSqlBaseValue"
                /// Recurses into the creation of new INSERT statements for non-base types
                let WriteInsertStatementForNonBaseType (f,t) v =
                    match t with 
                    | SqlOption (SqlBaseType bi,_)  -> Set.empty
                    | SqlBaseType bi                -> Set.empty                    
                    | _                             -> WriteInsertStatementHelper (Some (name + "Id", "@" + name + "Id")) (name + f) t v
                /// Writes a Sql table row to the string builder
                let WriteTableRow fts vs = 
                    /// Augment the values with the type information
                    let WriteField (s:string) = Append "["; Append s; Append "]"
                    let WriteBaseValue (s:string) = Append s
                    // b is true if writing field names, false if writing values
                    let rec WriteBaseValues b first fts vs  = 
                        match fts,vs with 
                        | (f,t) :: rest_fts, v :: rest_vs   -> 
                            if IsSqlBaseValue f t then 
                                if not first then Append "," 
                                if b then WriteField f else WriteBaseValue (GetSqlBaseValue t v)
                                WriteBaseValues b false rest_fts rest_vs
                            else
                                WriteBaseValues b first rest_fts rest_vs
                        | _                                 -> ()
                    let WriteBaseIdAndValues b fts vs  = 
                        match baseIdInfo with 
                        | Some (f,v) -> 
                            if b then WriteField f else WriteBaseValue v
                            WriteBaseValues b false fts vs
                        | None ->
                            WriteBaseValues b true fts vs

                    /// Create the INSERT into string together with the Id line
                    Append "INSERT INTO ["; Append name; Append "] ("; WriteBaseIdAndValues true fts vs; 
                    Append ") VALUES ("; WriteBaseIdAndValues false fts vs; Append ")\r\n"
                    Append "SET @"; Append name; Append "Id = @@IDENTITY\r\n";

                    /// Recurse for all fields that are not base types
                    List.fold2 (fun acc ft v -> Set.union acc (WriteInsertStatementForNonBaseType ft v)) (Set.singleton name) fts vs
                
                match sqlType with 
                | SqlRecord (fts,reader)     -> 
                    /// Gets the list of values for a record type (either it was a record value or a tuple value)
                    let vs = reader v 
                    WriteTableRow fts (vs |> Array.toList)
                | SqlArray t                -> 
                    /// For an array, match the array and iterate the insert statement of the inner values 
                    match v with 
                    | :? System.Array as va when va.Rank = 1    -> 
                        let vs : obj array = Array.init va.Length (fun i -> va.GetValue(i))
                        vs |> Array.fold (fun acc v -> Set.union acc (WriteInsertStatementHelper baseIdInfo name t (box v))) Set.empty
                    | _                     -> Set.empty
                | SqlOption (t,reader)      ->
                    /// For an option at this level, recurse if there is data
                    match v with 
                    | null                  -> Set.empty
                    | _                     -> WriteInsertStatementHelper baseIdInfo name t (reader v)
                | SqlBaseType bi            -> 
                    /// A base type is like a record with the fixed field "Value"
                    WriteTableRow [("Value", SqlBaseType bi)] [v]
             
            WriteInsertStatementHelper None baseTableName sqlType v

        {   
            new ISqlCommandSchema<_> with 
                member this.Create ()       = 
                    CreateStatement + "\r\n" + DropCreateInsertEnumStatements
                member this.Drop ()       = 
                    DropStatement 
                member this.Insert (x:'a)   = 
                    let insertIntoText = new StringBuilder (100)              
                    /// Get the set of variables     
                    let keyVariables = WriteInsertStatement insertIntoText (box x) 
                    /// Setup a fixed string builder that contains the DECLARE commands
                    let declareText = new StringBuilder (100)
                    /// A helper function to print strings via the DECLARE string builder
                    let Append2 (s:string) = declareText.Append (s) |> ignore
                    keyVariables |> Seq.iter (fun s -> Append2 "DECLARE @"; Append2 s; Append2 "Id bigint\r\n")
                    declareText.ToString () + insertIntoText.ToString ()
        }

    //------------------------------------------------------------------------
    // SQL Type to a DataSet interpreter
    //------------------------------------------------------------------------

    /// Builds an IDataSetSchema instance from an Sql type
    let SqlTypeToIDataSetSchema sqlType baseTableName enumTables = 
        /// The data set that holds instances of this type
        let dataSet = new DataSet ()
        
        /// Creates the in-memory data tables from a SQL type
        let CreateDataSet () = 
            /// A helper function to recursively build the data tables in the dataset 
            let rec CreateDataTablesHelper (parentKey,childKey) tableName sqlType = 

                /// Creates a data table with a given name and possibly updates it in the DataSet (in case we want to re-create it with the right keys)
                let UpdateDataTables name fts = 
                    /// Checks if the fields of a record are all "basic", that is, they never spawn a new table creation in which case
                    /// we do not need an Id column
                    let SimpleRecord fts =
                        fts |> List.forall (fun (_,ty) -> 
                            match ty with 
                            | SqlOption (SqlBaseType _,_)   -> true 
                            | SqlBaseType _                 -> true 
                            | _                             -> false
                        )
                    /// The seed for this table (either 1L or taken from the table to be deleted)
                    let seed = 
                        if not (SimpleRecord fts) && dataSet.Tables.Contains name then
                            let dt = dataSet.Tables.Item (name)
                            match dt.Rows.Count with 
                            | 0 -> (dt.Columns.Item (0)).AutoIncrementSeed
                            | _ -> let lastRow = dt.Rows.Item (dt.Rows.Count - 1)
                                   (lastRow.Item (0) :?> int64) + 1L
                        else
                            1L
                            
                    /// Generate a new data table                                 
                    let dataTable = new DataTable (name)
                    /// Insert the data table immediately 
                    if dataSet.Tables.Contains (name) then dataSet.Tables.Remove (name)
                    dataSet.Tables.Add (dataTable)
                    
                    /// Generate the primary key column, if necessary
                    let primaryKey = 
                        if not (SimpleRecord fts) then 
                            let dc = new DataColumn (name + "Id", typeof<int64>, ColumnMapping=MappingType.Attribute, AllowDBNull=false, AutoIncrement=true, AutoIncrementStep=1L, AutoIncrementSeed=seed)
                            dc |> dataTable.Columns.Add
                            dataTable.PrimaryKey <- [| dc |]
                            Some (dc)
                        else
                            None

                    /// Writes the SQL base type information (if there is any) or None
                    let CreateDataTableForNonBaseTypes (f,t) =
                        match t with 
                        | SqlOption (SqlBaseType bi,_)  -> ()
                        | SqlBaseType bi                -> ()
                        | _                             -> let key = Some (new DataColumn (tableName + "Id", typeof<int64>, ColumnMapping=MappingType.Attribute, AllowDBNull=false))
                                                           CreateDataTablesHelper (primaryKey, key) (tableName + f) t
                    /// Get the SQL base type information (if there is any) or None
                    let GetSqlBaseTypeInfo (f,t) =
                        match t with 
                        | SqlOption (SqlBaseType bi,_)  -> Some (new DataColumn (f, bi.DotNetType, ColumnMapping=MappingType.Attribute, AllowDBNull=true))
                        | SqlBaseType bi                -> Some (new DataColumn (f, bi.DotNetType, ColumnMapping=MappingType.Attribute, AllowDBNull=false, DefaultValue=bi.DefaultDotNetValue))
                        | _                             -> None

                    /// Insert the base table Id column, if necessary
                    do
                        match childKey with
                        | Some (childColumn) -> 
                            childColumn |> dataTable.Columns.Add
                            dataTable.Constraints.Add (new ForeignKeyConstraint (parentKey |> Option.get, childColumn, AcceptRejectRule = AcceptRejectRule.Cascade, DeleteRule = Rule.Cascade, UpdateRule = Rule.Cascade)) |> ignore
                            dataSet.Relations.Add (parentKey |> Option.get, childColumn, Nested=true) |> ignore
                        | None      -> 
                            ()
                        
                    /// Insert the base type columns                        
                    fts |> List.choose GetSqlBaseTypeInfo |> List.iter dataTable.Columns.Add
                    /// Insert the non-base type data tables
                    fts |> List.iter CreateDataTableForNonBaseTypes

                
                /// Main recursion
                match sqlType with 
                | SqlRecord (fts,_)         -> UpdateDataTables tableName fts
                | SqlArray t                -> CreateDataTablesHelper (parentKey, childKey) tableName t
                | SqlOption(t,_)            -> CreateDataTablesHelper (parentKey, childKey) tableName t
                | SqlBaseType bi            -> UpdateDataTables tableName ["Value", SqlBaseType bi]

            /// Disable all constraints and delete them                
            dataSet.Relations.Clear ()
            dataSet.Tables |> Seq.cast |> Seq.toList |> List.rev |> List.iter (fun (dt:DataTable) -> dt.Constraints.Clear ())
            
            /// Add the new data tables (and possibly constraints and relations)
            CreateDataTablesHelper (None, None) baseTableName sqlType

        /// Inserts a single data item into the dataset
        let Insert v = 
            /// The recursive function that inserts into a single table (and possibly recurses, if the type was complex)
            let rec InsertHelper parentRow (name:string) sqlType v = 
                /// Writes a data row into the current table 
                let WriteRow fts vs = 
                    /// Get the current table to insert into
                    let dataTable = dataSet.Tables.Item (name)
                    /// Allocate the new data row
                    let dataRow = dataTable.NewRow ()
                    /// Indicates whether or not the table has an Id field
                    let hasIdField = (dataTable.Columns.Item (0)).AutoIncrement
                    /// Determine if we have to skip an auto-increment column
                    let startIndex = if hasIdField then 1 else 0

                    try
                        /// Fill the base table Id field the delivered Id, if necessary
                        let startIndex = 
                            match parentRow with
                            | Some (pr) -> dataRow.SetParentRow (pr); startIndex + 1
                            | None      -> startIndex
                        /// Add the row to the table
                        dataRow |> dataTable.Rows.Add
                        /// A temporary copy of the row
                        let row = Array.zeroCreate dataRow.ItemArray.Length
                        
                        /// Get the .Net type value (if there is any), else returns None
                        let GetDotNetBaseValue t v =
                            match t with 
                            | SqlOption (SqlBaseType bi,reader) -> 
                                match v with 
                                | null  -> Some (null)
                                | _     -> Some (bi.DotNetValue (reader v))
                            | SqlBaseType bi                    -> Some (bi.DotNetValue v)
                            | _                                 -> None
                        /// Recurses into the creation of new datarows for non-base types
                        let InsertForNonBaseType (f,t) v =
                            if hasIdField then
                                InsertHelper (Some (dataRow)) (name + f) t v
                            else
                                failwith "InsertForNonBaseType: Id column not generated!"

                        List.fold2 (fun (i:int) (f,t) v -> 
                            match GetDotNetBaseValue t v with
                            | Some (d)  ->
                                row.[i] <- d
                                i + 1
                            | None      -> 
                                InsertForNonBaseType (f,t) v
                                i
                        ) startIndex fts vs |> ignore                         

                        /// Copy the values into the data row
                        dataRow.ItemArray <- row
                    with
                    | e -> 
                        printf "Rejecting: %s\n" e.Message
                        dataSet.EnforceConstraints <- true
                        dataRow.Delete ()
                        dataSet.EnforceConstraints <- false
                    

                match sqlType with 
                | SqlRecord (fts,reader)     -> 
                    /// Gets the list of values for a record type (either it was a record value or a tuple value)
                    WriteRow fts (v |> reader |> Array.toList)
                | SqlArray t                -> 
                    /// For an array, match the array and iterate the insert statement of the inner values 
                    match v with 
                    | :? System.Array as va when va.Rank = 1    -> 
                        let vs : obj array = Array.init va.Length (fun i -> va.GetValue (i))
                        vs |> Array.iter (fun v -> InsertHelper parentRow name t (box v))
                    | _                     -> ()
                | SqlOption (t,reader)      ->
                    /// For an option at this level, recurse if there is data
                    match v with 
                    | null                  -> ()
                    | _                     -> InsertHelper parentRow name t (reader v)
                | SqlBaseType bi            -> 
                    /// A base type is like a record with the fixed field "Value"
                    WriteRow [("Value", SqlBaseType bi)] [v]
             
            InsertHelper None baseTableName sqlType v

        /// Create the data set (for the first time)
        CreateDataSet ()
        
        /// Return the schema
        {   
            new IDataSetSchema<_> with 
                member this.Clear ()        = CreateDataSet ()
                member this.Insert (x:'a)   = Insert (box x)
                member this.Dataset         = dataSet
        }

    //------------------------------------------------------------------------
    // SQL Schema builder
    //------------------------------------------------------------------------

    /// Builds a schema of SQL commands for creating and inserting into an Sql database
    let build<'a> (server, database, prefix) =
        /// The .Net type of the type variable
        let dotNetType = typeof<'a>
        /// The compiled type
        let (sqlType,enumTables) = DotNetTypeToSqlType dotNetType
        /// The name of the base table
        let baseTableName = TypeName dotNetType
        /// The strongly typed SQL command schema
        let scs = SqlTypeToISqlCommandSchema sqlType (prefix + baseTableName) enumTables
        
        /// Open the connection
        let conn = new SqlConnection ("Initial Catalog = " + database + "; Data Source = " + server + "; Integrated Security = SSPI")
        conn.Open ()
        /// Define a general Sql command execution routine
        let Execute s = 
            try
                let command = conn.CreateCommand (CommandText = s, CommandTimeout = 0)
                command.ExecuteNonQuery () |> ignore 
            with
            | e -> printf "Warning: %s\n" e.Message
        {
            new ISqlSchema<_> with 
                member this.Create ()       = scs.Create ()     |> Execute
                member this.Drop ()         = scs.Drop ()       |> Execute
                member this.Insert (x:'a)   = scs.Insert (x)    |> Execute
        }
    
    /// Helper function for bulkBuild and bulkBuildObj                    
    let (*private*) bulkBuilder dotNetType (server, database, prefix) =        
        /// The compiled type
        let (sqlType,enumTables) = DotNetTypeToSqlType dotNetType
        /// The name of the base table
        let baseTableName = TypeName dotNetType
        /// The strongly typed SQL command schema
        let scs = SqlTypeToISqlCommandSchema sqlType (prefix + baseTableName) enumTables
        /// The strongly typed Dataset schema
        let sd = SqlTypeToIDataSetSchema sqlType (prefix + baseTableName) enumTables
        
        /// Open the connection
        let connStr = "Initial Catalog = " + database + "; Data Source = " + server + "; Integrated Security = SSPI"
        let conn = new SqlConnection (connStr)
        conn.Open ()
        /// The bulk copy engine
        let bulkCopy = new SqlBulkCopy (connStr, SqlBulkCopyOptions.KeepIdentity, BulkCopyTimeout = 0)
        /// Define a general Sql command execution routine
        let Execute s = 
            try
                let command = conn.CreateCommand (CommandText = s, CommandTimeout = 0)
                command.ExecuteNonQuery () |> ignore 
            with
            | e -> printf "Warning: %s\n" e.Message

        /// First drop the tables and re-create all the tables from scratch    
        scs.Drop ()     |> Execute
        scs.Create ()   |> Execute 
        (sd, bulkCopy)
                    
    /// Flushes current DataSet values to Database
    let (*private*) Flush ((sd:IDataSetSchema<'a>),(bulkCopy:SqlBulkCopy)) =
        lock sd.Dataset (fun () ->
                        sd.Dataset.Tables 
                        |> Seq.cast
                        |> Seq.iter (fun (dt:DataTable) -> 
                            bulkCopy.DestinationTableName <- dt.TableName
                            bulkCopy.WriteToServer (dt)
                        )
                        sd.Clear ()
                    )        
        
    /// Builds a strongly typed schema for bulk inserting into an Sql database
    let bulkBuild<'a> (server, database, prefix) =
        /// The .Net type of the type variable
        let dotNetType = typeof<'a>
        let sd, bulkCopy = bulkBuilder dotNetType (server, database, prefix)
        {
            new IBulkInsertSqlSchema<_> with 
                member this.Insert (x:'a)   = lock sd.Dataset (fun () -> sd.Insert x)
                member this.Flush ()        = Flush (sd,bulkCopy)
                    
        }        
        
    /// Builds a schema for bulk inserting into an Sql database
    let bulkBuildObj dotNetType  (server, database, prefix) =              
        let sd, bulkCopy = bulkBuilder dotNetType (server, database, prefix)
        {
            new IBulkInsertSqlSchema<_> with 
                member this.Insert (x:obj)   = lock sd.Dataset (fun () -> sd.Insert x)
                member this.Flush ()        =  Flush (sd, bulkCopy)
        }              
        
