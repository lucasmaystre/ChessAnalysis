//--------------------------------------------------------------
// Program.fs           Chess analysis for the NIPS paper
//
// 2007/2008 written by Ralf Herbrich
// Microsoft Research Ltd
//--------------------------------------------------------------

#light

open System
open System.IO
open System.Collections.Generic
open MSRC.Inference.Distributions
open MSRC.Inference.Distributions.Gaussians
open MSRC.Chess
open MSRC.Tools.Sql.SqlSchema

/// The skill belief of a player in a year
type PlayerData =
    {
        /// Name of the player
        PlayerName  : string
        /// Year of the analysis
        Year        : int 
        /// Mean skill
        Mu          : float
        /// Skill uncertainty
        Sigma       : float
    }

/// The skill and draw margin belief of a player in a year
type ExtendedPlayerData =
    {
        /// Name of the player
        PlayerName      : string
        /// Year of the analysis
        Year            : int 
        /// Mean skill
        SkillMu         : float
        /// Skill uncertainty
        SkillSigma      : float
        /// Mean draw margin
        DrawMarginMu    : float
        /// Draw Margin uncertainty
        DrawMarginSigma : float
    }

/// The Log Evidence as a function of the Beta and Tau factor
type ModelData =
    {
        /// Beta/Sigma0
        BetaFactor      : float
        /// Tau/Sigma0
        TauFactor       : float
        /// Log Evidence
        LogEvidence : float
        /// Name of the result table
        ResultTableName : string
    }

/// The Log Evidence as a function of the Mu factor and Tau factor
type DrawModelData =
    {
        /// Mu/Beta
        MuFactor        : float
        /// Tau/Sigma0
        TauFactor       : float
        /// Log Evidence
        LogEvidence : float
        /// Name of the result table
        ResultTableName : string
    }

/// Type of models for the Chess data
type Model =
    | FixedDrawMargin
    | FixedDrawMargin2
    | FixedDrawMarginADFOverYears
    | FixedDrawMarginADF
    | VariableDrawMargin

/// Types of analysis for the Chess data
type Analysis =
    | SingleRun
    | ModelSelection
    
/// Verbosity level
let verbose = ref true
/// The type of analysis to perform
let analysis = ref SingleRun
/// The type of model to use
let model = ref FixedDrawMargin
/// The name of the CSV data file
let csvFile = ref "ChessBase.csv"
/// The name of the SQL server to use
let server = ref "camresapga01"
/// The name of the SQL database to use 
let database = ref "ChessBase"
/// The prefix to be used for the result table
let prefix = ref "Result"
/// The mean of the prior skill belief
let muSkill = ref 1200.0
/// The standard deviation of the prior skill belief
let sigmaSkill = ref 400.0
/// The mean of the prior draw margin belief
let muDrawMargin = ref 300.0
/// The standard deviation of the prior draw margin belief
let sigmaDrawMargin = ref 100.0
/// The standard deviation of the performance distribution
let beta = ref 600.0
/// The standard deviation of the skill dynamics distribution
let tauSkill = ref 40.0
/// The standard deviation of the draw margin dynamics distribution
let tauDrawMargin = ref 10.0
/// The number of games to take
let noGames = ref None
/// Should results be saved
let notSave = ref false
/// Maximum deviation for any marginal to define convergence
let delta = ref 0.01

/// A small function which returns the name for the current model
let ModelName () = 
    match !model with 
    | FixedDrawMargin               -> "FixedDrawMargin" 
    | FixedDrawMargin2              -> "FixedDrawMargin2" 
    | FixedDrawMarginADFOverYears   -> "FixedDrawMarginADFOverYears" 
    | FixedDrawMarginADF            -> "FixedDrawMarginADF" 
    | VariableDrawMargin            -> "VariableDrawMargin"
    
/// A small function which returns the name for the current analysis
let AnalysisName () = 
    match !analysis with 
    | SingleRun -> "SingleRun" 
    | ModelSelection -> "ModelSelection"
    

/// The list of arguments of this application
let argspec = 
    [   
        ("-no-safe",Arg.Set notSave,                                    "Does not save results predictions (default: off)")
        ("-q",      Arg.Clear verbose,                                  "Verbosity level off; only works on single runs (default: on)")
        ("-N",      Arg.Int (fun n -> noGames := Some (n)),             "First N games only (default: ALL)")
        ("-delta",  Arg.Float (fun aDelta -> delta := aDelta),          "Maximum deviation in any marginal for convergence (default: 0.01)")
        ("-muS",    Arg.Float (fun aMu -> muSkill := aMu),              "Mean of prior skill belief (default: 1200)")
        ("-sigmaS", Arg.Float (fun aSigma -> sigmaSkill := aSigma),     "Standard deviation of prior skill belief (default: 400)")
        ("-muD",    Arg.Float (fun aMu -> muDrawMargin := aMu),         "Mean of prior draw margin belief (default: 300)")
        ("-sigmaD", Arg.Float (fun aSigma -> sigmaDrawMargin := aSigma),"Standard deviation of prior draw margin belief (default: 100)")
        ("-beta",   Arg.Float (fun aBeta -> beta := aBeta),             "Standard deviation of performance distr. (default: 600)")
        ("-tauS",   Arg.Float (fun aTau -> tauSkill := aTau),           "Standard deviation of skill dynamics distr. (default: 40)")
        ("-tauD",   Arg.Float (fun aTau -> tauDrawMargin := aTau),      "Standard deviation of draw margin dynamics distr. (default: 10)")
        ("-server", Arg.String (fun aServer -> server := aServer),      "SQL server name for output (default: 'camresapga01')")
        ("-prefix", Arg.String (fun aPrefix -> prefix := aPrefix),      "Tablename prefix for output (default: 'Result')")
        ("-db",     Arg.String (fun aDB -> database := aDB),            "Name of the database for output (default: 'ChessBase')")
        ("-mf",     Arg.Unit (fun () -> model := FixedDrawMargin),      "Fixed draw margin (default)")
        ("-mf2",    Arg.Unit (fun () -> model := FixedDrawMargin2),     "Fixed draw margin with two factors for draw")
        ("-maf",    Arg.Unit (fun () -> model := FixedDrawMarginADFOverYears),  "Fixed draw margin with ADF (iterate per year)")
        ("-maf2",   Arg.Unit (fun () -> model := FixedDrawMarginADF),   "Fixed draw margin with pure ADF")
        ("-mv",     Arg.Unit (fun () -> model := VariableDrawMargin),   "Variable draw margin")
        ("-as",     Arg.Unit (fun () -> analysis := SingleRun),         "Single run (default)")
        ("-am",     Arg.Unit (fun () -> analysis := ModelSelection),    "Model selection")
    ]

/// Saves the result of a fixed draw margin analysis to a SQL table
let SaveFixedDrawMarginResults (results:Dictionary<string,SortedList<int,Gaussian<_>>>) tablePrefix = 
    if !notSave then
        () 
    else
        let sqlSchema = bulkBuild (!server, !database, tablePrefix)
        results |> Seq.iter (fun kvp ->
            let playerName = kvp.Key
            kvp.Value |> Seq.iter (fun kvp -> 
                sqlSchema.Insert
                    {
                        new PlayerData
                        with PlayerName  = playerName
                        and  Year        = kvp.Key
                        and  Mu          = (float kvp.Value.Mu)
                        and  Sigma       = (float kvp.Value.Sigma)
                    }
            )
        )
        sqlSchema.Flush ()

/// Saves the result of a variable draw margin analysis to a SQL table
let SaveVariableDrawMarginResults (results:Dictionary<string,SortedList<int,Gaussian<_>*Gaussian<_>>>) tablePrefix = 
    if !notSave then 
        ()
    else
        let sqlSchema = bulkBuild (!server, !database, tablePrefix)
        results |> Seq.iter (fun kvp ->
            let playerName = kvp.Key
            kvp.Value |> Seq.iter (fun kvp -> 
                sqlSchema.Insert
                    {
                        new ExtendedPlayerData
                        with PlayerName         = playerName
                        and  Year               = kvp.Key
                        and  SkillMu            = (float (fst kvp.Value).Mu)
                        and  SkillSigma         = (float (fst kvp.Value).Sigma)
                        and  DrawMarginMu       = (float (snd kvp.Value).Mu)
                        and  DrawMarginSigma    = (float (snd kvp.Value).Sigma)
                    }
            )
        )
        sqlSchema.Flush ()

[<Measure>] type ELOPoints

do
    /// Some useful information on the screen
    printfn "Chess Analysis program v 1.0"
    printfn "2007 written by Ralf Herbrich"
    printfn "Microsoft Research Ltd."
    printfn ""

    /// Parse the arguments
    Arg.parse argspec (fun fileName -> csvFile := fileName) (sprintf "USAGE: %s [params] [<csv-file>]" (System.IO.Path.GetFileName Sys.argv.[0]))

    /// Allocate the chess dataset
    let cds = ChessDataset<ELOPoints> ()
    
    printfn "[Reading the dataset]"
    cds.ReadFromCSVFile (!csvFile,!noGames)

    match !analysis with 
    | SingleRun -> 
        /// Output some useful info and get the empirical draw probability 
        printfn "[Single run on the dataset using model '%s']" (ModelName())
        let p = float (cds.MatchSequence |> Seq.filter (fun mtch -> mtch.Outcome = MatchOutcome.Draw) |> Seq.length) / float (cds.MatchSequence |> Seq.length)
        let chessAnalysisParameters = ChessAnalysisParameters<ELOPoints> (PriorMuSkill = !muSkill * 1.0<ELOPoints>, PriorSigmaSkill = !sigmaSkill * 1.0<ELOPoints>, PriorMuDrawMargin = !muDrawMargin * 1.0<ELOPoints>, PriorSigmaDrawMargin = !sigmaDrawMargin * 1.0<ELOPoints>, Beta = !beta * 1.0<ELOPoints>, TauSkill = !tauSkill * 1.0<ELOPoints>, TauDrawMargin = !tauDrawMargin * 1.0<ELOPoints>, DrawProbability = p)
        printfn "[Parameters used]"
        printfn "%O" chessAnalysisParameters
        
        match !model with 
        | FixedDrawMargin ->
            let (results,logZ) = cds.FixedDrawMarginAnalyse chessAnalysisParameters !verbose
            SaveFixedDrawMarginResults results (!prefix ^ ModelName())
        | FixedDrawMargin2 ->
            let (results,logZ) = cds.FixedDrawMarginAnalyse2 chessAnalysisParameters !verbose
            SaveFixedDrawMarginResults results (!prefix ^ ModelName())
        | FixedDrawMarginADFOverYears -> 
            let results = cds.FixedDrawMarginADFAnalyse chessAnalysisParameters true !verbose
            SaveFixedDrawMarginResults results (!prefix ^ ModelName())
        | FixedDrawMarginADF -> 
            let results = cds.FixedDrawMarginADFAnalyse chessAnalysisParameters false !verbose
            SaveFixedDrawMarginResults results (!prefix ^ ModelName())
        | VariableDrawMargin ->
            let (results,logZ) = cds.VariableDrawMarginAnalyse chessAnalysisParameters !verbose
            SaveVariableDrawMarginResults results (!prefix ^ ModelName())
            ()
            // SaveFixedDrawMarginResults results !prefix
    | ModelSelection -> 
        /// Output some useful info and get the empirical draw probability 
        printfn "[Model selection run on the dataset using model '%s']" (ModelName())
        let p = float (cds.MatchSequence |> Seq.filter (fun mtch -> mtch.Outcome = MatchOutcome.Draw) |> Seq.length) / float (cds.MatchSequence |> Seq.length)
        
        match !model with 
        | FixedDrawMargin ->
            /// Build the result schema
            let resSchema = build (!server, !database, (!prefix ^ ModelName ()))
            resSchema.Drop ()
            resSchema.Create ()

            let p = float (cds.MatchSequence |> Seq.filter (fun mtch -> mtch.Outcome = MatchOutcome.Draw) |> Seq.length) / float (cds.MatchSequence |> Seq.length)
            /// Iterate over the range of all beta/tau values
            let betaTauRange = [| for betaF in 2 .. 2 .. 20 do for tauF in 5 .. 5 .. 50 -> ((float betaF)/10.0,(float tauF)/100.0) |]
            /// Map the range to the log normalisation constant
            betaTauRange |> Array.iteri (fun i (betaF,tauF) -> 
                let chessAnalysisParameters = ChessAnalysisParameters<ELOPoints> (PriorMuSkill = !muSkill * 1.0<ELOPoints>, PriorSigmaSkill = !sigmaSkill * 1.0<ELOPoints>, Beta = !sigmaSkill*betaF * 1.0<ELOPoints>, TauSkill = !sigmaSkill*tauF * 1.0<ELOPoints>, DrawProbability = p)
                let (results,logZ) = cds.FixedDrawMarginAnalyse chessAnalysisParameters false
                printf "%f,%f,%f\n" betaF tauF logZ

                /// Output the dataset to SQL
                let tablePrefix = sprintf "%s%d" !prefix i
                SaveFixedDrawMarginResults results tablePrefix
                
                resSchema.Insert 
                    {
                        new  ModelData
                        with BetaFactor      = betaF
                        and  TauFactor       = tauF
                        and  LogEvidence     = logZ
                        and  ResultTableName = tablePrefix
                    }
            )
        | FixedDrawMargin2 ->
            printfn "Not implemented!"
        | FixedDrawMarginADFOverYears -> 
            printfn "Not implemented!"
        | FixedDrawMarginADF -> 
            printfn "Not implemented!"
        | VariableDrawMargin ->
            /// Build the result schema
            let resSchema = build (!server, !database, (!prefix ^ ModelName ()))
            resSchema.Drop ()
            resSchema.Create ()

            let p = float (cds.MatchSequence |> Seq.filter (fun mtch -> mtch.Outcome = MatchOutcome.Draw) |> Seq.length) / float (cds.MatchSequence |> Seq.length)
            /// Iterate over the range of all beta/tau values
            let muDrawMarginTauDrawMarginRange = [| for muDrawMarginF in 5 .. 1 .. 10 do for tauDrawMarginF in 5 .. 5 .. 20 -> ((float muDrawMarginF)/10.0,(float tauDrawMarginF)/100.0) |]
            /// Map the range to the log normalisation constant
            muDrawMarginTauDrawMarginRange |> Array.iteri (fun i (muDrawMarginF,tauDrawMarginF) -> 
                let sigmaDrawMargin = !beta / 6.0
                let chessAnalysisParameters = ChessAnalysisParameters<ELOPoints> (PriorMuSkill = !muSkill * 1.0<ELOPoints>, PriorSigmaSkill = !sigmaSkill * 1.0<ELOPoints>, PriorMuDrawMargin = !beta*muDrawMarginF * 1.0<ELOPoints>, PriorSigmaDrawMargin = sigmaDrawMargin * 1.0<ELOPoints>, Beta = !beta * 1.0<ELOPoints>, TauSkill = !tauSkill * 1.0<ELOPoints>, TauDrawMargin = tauDrawMarginF*sigmaDrawMargin * 1.0<ELOPoints>, DrawProbability = p)
                let (results,logZ) = cds.VariableDrawMarginAnalyse chessAnalysisParameters false
                printf "%f,%f,%f\n" muDrawMarginF tauDrawMarginF logZ

                /// Output the dataset to SQL
                let tablePrefix = sprintf "%s%d" !prefix i
                SaveVariableDrawMarginResults results tablePrefix
                
                resSchema.Insert 
                    {
                        new DrawModelData
                        with MuFactor           = muDrawMarginF
                        and  TauFactor          = tauDrawMarginF
                        and  LogEvidence        = logZ
                        and  ResultTableName    = tablePrefix
                    }
            )

