//--------------------------------------------------------------
// Chess.fs           Chess analysis library
//
// 2007/2008 written by Ralf Herbrich
//
// Copyright (c) 2002-2011 Microsoft Research Ltd.
//
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
//
//--------------------------------------------------------------

module MSRC.Chess

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics
open MSRC.Inference
open MSRC.Inference.Collections
open MSRC.Inference.Distributions
open MSRC.Inference.Distributions.Gaussians
open MSRC.Inference.Schedule
  
//----------------------------------------------------------------------------
// Chess specific routines
//----------------------------------------------------------------------------
[<Measure>] type ELOPoints

/// The possible match outcomes of a Chess match
type MatchOutcome = 
    | BlackWins = 0     
    | WhiteWins = 1
    | Draw = 2

/// A single match between two players
type Match =
    {
        /// Year in which the match happened
        Year            : int
        /// Id of the White player
        WhitePlayerId   : int
        /// Id of the Black player
        BlackPlayerId   : int
        /// Outcome of the match
        Outcome         : MatchOutcome
    }

/// The parameter for an analysis of a Chess outcome data
type ChessAnalysisParameters () = 
    let mutable priorMuSkill = 1200.0<ELOPoints>
    let mutable priorSigmaSkill = 400.0<ELOPoints>
    let mutable beta = 200.0<ELOPoints>
    let mutable tauSkill = 40.0<ELOPoints>
    let mutable tauDrawMargin = 20.0<ELOPoints>
    let mutable priorMuDrawMargin = 300.0<ELOPoints>
    let mutable priorSigmaDrawMargin = 100.0<ELOPoints>
    let mutable drawProbability = 0.3
    let mutable delta = 1e-2</ELOPoints>

    /// The mean of the prior belief of a Chess player's skill
    member this.PriorMuSkill with get () = priorMuSkill and set v = priorMuSkill <- v        
    /// The standard deviation of the prior belief of a Chess player's skill
    member this.PriorSigmaSkill with get () = priorSigmaSkill and set v = priorSigmaSkill <- v        
    /// The standard deviation of the prior belief of a Chess player's draw margin
    member this.PriorSigmaDrawMargin with get () = priorSigmaDrawMargin and set v = priorSigmaDrawMargin <- v        
    /// The mean of the prior belief of a Chess player's draw margin
    member this.PriorMuDrawMargin with get () = priorMuDrawMargin and set v = priorMuDrawMargin <- v        
    /// The standard deviation of the performance distribution
    member this.Beta with get () = beta and set v = beta <- v        
    /// The standard deviation of the skill dynamics distribution
    member this.TauSkill with get () = tauSkill and set v = tauSkill <- v        
    /// The standard deviation of the draw margin dynamics distribution
    member this.TauDrawMargin with get () = tauDrawMargin and set v = tauDrawMargin <- v        
    /// The draw probability between any two players
    member this.DrawProbability with get () = drawProbability and set v = drawProbability <- v  
    /// The precision with which the analysis should be carried out
    member this.Delta with get () = delta and set v = delta <- v  
    /// Prints the Chess-analysis parameters to a string
    override this.ToString () = 
        sprintf "Skill Prior Mu\t\t= %f\nSkill Prior Sigma\t= %f\nDrawMargin Prior Mu\t= %f\nDrawMargin Prior Sigma\t= %f\nBeta\t\t\t= %f\nTau Skill\t\t= %f\nDrawMargin Mu\t\t= %f\nDraw Probability\t= %f\nDelta\t\t\t= %f\n"
            (float priorMuSkill) (float priorSigmaSkill) (float priorMuDrawMargin) (float priorSigmaDrawMargin) (float beta) 
            (float tauSkill) (float tauDrawMargin) (float drawProbability) (float delta)

/// A pair of indicies for skills and draw margins
type SkillDrawMargin =
    struct
        /// The index of the skill variable
        val SkillIdx        : int
        /// The index of the draw margin variable
        val DrawMarginIdx   : int
    end
    with
        new (s,d) = { SkillIdx = s; DrawMarginIdx = d }
    end

/// A chess data-set encapsulates all historical data of chess outcomes 
type ChessDataset () = 
    /// The internal mapping of player names to player Ids
    let PlayerNamesToPlayerId = new Dictionary<_,_> ()
    /// The internal mapping of player Ids to player names
    let PlayerIdToPlayerName = new Dictionary<_,_> ()
    /// The array of matches order by year
    let mutable Matches = [| |]
    
    /// Gets a unique player Id for a given player name
    let GetPlayerId (name:string) = 
        if PlayerNamesToPlayerId.ContainsKey name then
            PlayerNamesToPlayerId.[name]
        else 
            let id = PlayerNamesToPlayerId.Count
            PlayerNamesToPlayerId.Add (name, id)
            PlayerIdToPlayerName.Add (id, name)
            id

    /// The start and end year of each player
    let playerStartEndYear = new Dictionary<_,_> ()
    
    /// Recomputes the start and end year of each player
    let RecomputeStartEndYear () = 
        ///  Updates the start and end year of a single player given a year he/she has played
        let UpdatePlayerStartEndYear id year = 
            if not (playerStartEndYear.ContainsKey id) then
                playerStartEndYear.Add (id, (year, year))
            else
                let (s,e) = playerStartEndYear.[id]
                playerStartEndYear.[id] <- (min s year, max e year)
            
        playerStartEndYear.Clear ()
        Matches |> Array.iter (fun mtch -> 
            UpdatePlayerStartEndYear mtch.WhitePlayerId mtch.Year
            UpdatePlayerStartEndYear mtch.BlackPlayerId mtch.Year
        )
            
    /// Opens a CSV file of chess match outcomes (each line is a chess match outcome) and builds the dataset
    member this.ReadFromCSVFile (fileName:string, noGames: int option) = 
        /// Clear all the player name mappings
        PlayerNamesToPlayerId.Clear ()
        PlayerIdToPlayerName.Clear ()
        
        /// Read all the data and store the records indexed by game_id in the playerGames dictionary
        let matches = 
            seq {
                use reader = new StreamReader (fileName)
                while not (reader.EndOfStream) do
                    let s = reader.ReadLine ()
                    let xs = s.Split [|','|] 
                    let year = xs.[1] |> Int32.Parse
                    let idWhitePlayer = GetPlayerId xs.[2]
                    let idBlackPlayer = GetPlayerId xs.[3]
                    let scoreWhitePlayer = xs.[4] |> Int32.Parse
                    let scoreBlackPlayer = xs.[5] |> Int32.Parse
                    let outcome = 
                        match scoreBlackPlayer - scoreWhitePlayer with 
                        | 0             -> MatchOutcome.Draw 
                        | x when x < 0  -> MatchOutcome.WhiteWins 
                        | _             -> MatchOutcome.BlackWins
                    
                    yield 
                        {
                            Year            = year
                            WhitePlayerId   = idWhitePlayer
                            BlackPlayerId   = idBlackPlayer
                            Outcome         = outcome                        
                        }
            }
            |> Seq.truncate (match noGames with None -> Int32.MaxValue | Some (n)  -> n)
            |> Seq.toArray
        
        /// Sort the matches by time
        matches |> Array.sortInPlaceWith (fun m1 m2 -> m1.Year - m2.Year)
        
        /// Update the internal storage
        Matches <- matches
        
        /// Re-compute the start/end year of each player
        RecomputeStartEndYear ()
        
    /// Returns an iteration throuh all the matches sorted by year
    member this.MatchSequence = Matches |> Array.toSeq
    
    /// Sequencey of player ids
    member this.PlayerIds = playerStartEndYear |> Seq.map (fun kvp -> kvp.Key)
            
    /// Gives the player name for a given player id
    member this.PlayerName id = PlayerIdToPlayerName.[id]

    /// Gives the player name for a given player id
    member this.PlayerStartEndYear id = playerStartEndYear.[id]
    
    /// Runs a full forward-backward factor graph analysis on a chess outcome dataset with a fixed draw margin
    member this.FixedDrawMarginAnalyse (param:ChessAnalysisParameters) (verbose:bool) = 
        /// Compute the internal parameters
        let priorBeliefPlayerMu = param.PriorMuSkill
        let priorBeliefPlayerVariance = param.PriorSigmaSkill * param.PriorSigmaSkill 
        let betaSquared = param.Beta * param.Beta
        let tauSquared = param.TauSkill * param.TauSkill
        
        /// Some useful output
        if verbose then printfn "[Building the factor graph]"
        
        /// The list of factors
        let factorList = FactorList ()
        
        /// The maximal accuracy that we require for convergence
        let delta = param.Delta
        
        /// Allocate the dictionary of all relevant variables
        let varBag = DistributionBag<_> (Gaussian<ELOPoints> (0.0<_>, 0.0<_>))
        
        /// Allocate new messages
        let GF = new GaussianFactors<ELOPoints> ()
        
        /// The dictionary of player skills per year per player
        let playerSkillVarIdxDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of dynamics factors
        let skillDynamicsFDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of prior factors
        let skillPriorFDict = Dictionary<_,_> (HashIdentity.Structural)
        
        /// Allocate all the variables of interest (per year, per playerId); their dynamics and their priors
        if verbose then 
            printfn "[Allocating the skill variables (%d variables)]" (this.PlayerIds |> Seq.fold (fun acc id -> let (s,e) = this.PlayerStartEndYear id in acc + (e-s+1)) 0)
        this.PlayerIds |> Seq.iter (fun id -> 
            let (s,e) = this.PlayerStartEndYear id
            for year = s to e do 
                playerSkillVarIdxDict.Add ((year,id),varBag.NewDistribution ())
                if year > s then 
                    skillDynamicsFDict.Add ((year,id),GF.GaussianLikelihoodFactor tauSquared (playerSkillVarIdxDict.[(year,id)],varBag) (playerSkillVarIdxDict.[(year-1,id)],varBag) |> factorList.AddFactor)
                else
                    skillPriorFDict.Add (id,GF.GaussianPriorFactor (priorBeliefPlayerMu,priorBeliefPlayerVariance) (playerSkillVarIdxDict.[(year,id)],varBag) |> factorList.AddFactor)
            done
        )
        
        /// Convert the probability of draw into an epsilon
        let epsilon = -sqrt (2.0 * betaSquared) * PhiInverse ((1.0 - param.DrawProbability) / 2.0)
        
        /// At first, send the skill priors of all players to the player variable nodes
        let priorSchedule = ScheduleSeq (skillPriorFDict |> Seq.map (fun kvp -> ScheduleStep (kvp.Value,0)))
        
        /// Create a dictionary of matches for each year
        if verbose then printfn "[Grouping the matches by year]"
        let matchesByYear = Dictionary<_,_> ()
        Matches |> Array.iter (fun mtch -> 
            if not (matchesByYear.ContainsKey mtch.Year) then 
                matchesByYear.Add (mtch.Year, new ResizeArray<_> ())
            (matchesByYear.[mtch.Year]).Add (mtch)
        )
        let minYear,maxYear = matchesByYear |> Seq.fold (fun (mn,mx) kvp -> (min mn kvp.Key, max mx kvp.Key)) (Int32.MaxValue,Int32.MinValue)
        
        /// Builds the schedule recursively (forward schedule before recursion and backwards schedule after recursion)
        if verbose then printfn "[Building the schedule year-by-year]"
        let rec BuildSchedule year = 
            /// First, build the schedule where all the dynamics factor send all the message forward in time
            let forwardDynamicsSchedule = 
                ScheduleSeq (skillDynamicsFDict 
                    |> Seq.choose (fun kvp -> let (y,_) = kvp.Key in if y = year then Some (kvp.Value) else None) 
                    |> Seq.map (fun update -> ScheduleStep (update, 0)) 
                    |> Seq.toArray 
                    |> Array.toSeq
                )
            
            /// Now build a loop schedule over all matches
            let dataScheduleSteps = new ResizeArray<_> ()
            
            /// Check that there is at least one match per year
            if matchesByYear.ContainsKey year then 
                matchesByYear.[year] |> Seq.iter (fun mtch -> 
                    let blackPerformance = varBag.NewDistribution ()
                    let whitePerformance = varBag.NewDistribution ()
                    let performanceDiff = varBag.NewDistribution ()
                    let likelBlackF = GF.GaussianLikelihoodFactor betaSquared (blackPerformance,varBag) (playerSkillVarIdxDict.[(year,mtch.BlackPlayerId)],varBag) |> factorList.AddFactor
                    let likelWhiteF = GF.GaussianLikelihoodFactor betaSquared (whitePerformance,varBag) (playerSkillVarIdxDict.[(year,mtch.WhitePlayerId)],varBag) |> factorList.AddFactor
                    let diffF = 
                        match mtch.Outcome with
                        | MatchOutcome.BlackWins    -> GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff,varBag) (blackPerformance,varBag) (whitePerformance,varBag) |> factorList.AddFactor
                        | MatchOutcome.WhiteWins    -> GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff,varBag) (whitePerformance,varBag) (blackPerformance,varBag) |> factorList.AddFactor
                        | MatchOutcome.Draw         -> GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff,varBag) (blackPerformance,varBag) (whitePerformance,varBag) |> factorList.AddFactor
                        | _                         -> failwith "Invalid match outcome"
                        
                    let matchF = 
                        match mtch.Outcome with
                        | MatchOutcome.BlackWins    -> GF.GaussianGreaterThanFactor epsilon (performanceDiff,varBag) |> factorList.AddFactor
                        | MatchOutcome.WhiteWins    -> GF.GaussianGreaterThanFactor epsilon (performanceDiff,varBag) |> factorList.AddFactor
                        | MatchOutcome.Draw         -> GF.GaussianWithinFactor epsilon (performanceDiff,varBag) |> factorList.AddFactor
                        | _                         -> failwith "Invalid match outcome"
                    dataScheduleSteps.Add (ScheduleStep (likelBlackF,0))
                    dataScheduleSteps.Add (ScheduleStep (likelWhiteF,0))
                    dataScheduleSteps.Add (ScheduleStep (diffF,0))
                    dataScheduleSteps.Add (ScheduleStep (matchF,0))
                    dataScheduleSteps.Add (ScheduleStep (diffF,1))
                    dataScheduleSteps.Add (ScheduleStep (diffF,2))
                    dataScheduleSteps.Add (ScheduleStep (likelBlackF,1))
                    dataScheduleSteps.Add (ScheduleStep (likelWhiteF,1))
                )
            let dataSchedule = ScheduleLoop (ScheduleSeq (dataScheduleSteps |> Seq.toList |> List.toSeq), delta)
            
            if verbose then printfn "\tFinished building schedule for year %d." year
            
            /// Get the schedule for the next years
            let nextYearSchedule = if (year < maxYear) then BuildSchedule (year+1) else ScheduleSeq (Seq.empty)

            /// Lastly, build the schedule where all the dynamics factor send all the message backward in time
            let backwardDynamicsSchedule = 
                ScheduleSeq (skillDynamicsFDict 
                    |> Seq.choose (fun kvp -> let (y,_) = kvp.Key in if y = year then Some (kvp.Value) else None) 
                    |> Seq.map (fun update -> ScheduleStep (update, 1)) 
                    |> Seq.toArray 
                    |> Array.toSeq
                )

            /// Now connect all the schedules and connect them
            ScheduleSeq ([| forwardDynamicsSchedule; dataSchedule; nextYearSchedule; dataSchedule; backwardDynamicsSchedule |] |> Array.toSeq)
            
        /// Build the full schedule ...
        let fullSchedule = ScheduleLoop (ScheduleSeq ([| priorSchedule ;BuildSchedule minYear |] |> Array.toSeq), delta)
        
        /// ... and run it
        if verbose then printfn "[Starting to run the schedule]"
        let delta = Run fullSchedule (if verbose then 1 else -1)
        if verbose then printfn "[Finished schedule with precision = %16.15f]" (float delta)
        
        /// Output the normalisation constant
        let logZ = factorList.LogNormalisation
        if verbose then printfn "[Log Normalisation = %16.15f]" logZ
        
        /// Build the list of skill per player 
        if verbose then printfn "[Building the player skill result data structure]"
        let playerSkillsPerPlayer = Dictionary<_,_> ()
        playerSkillVarIdxDict |> Seq.iter (fun kvp -> 
            let (year,id) = kvp.Key
            let playerName = this.PlayerName id
            if not (playerSkillsPerPlayer.ContainsKey playerName) then
                playerSkillsPerPlayer.Add (playerName, new SortedList<_,_> ())
            (playerSkillsPerPlayer.[playerName]).Add (year, varBag.[kvp.Value])
        )
        if verbose then printfn "[Finished the analysis]"
        
        (playerSkillsPerPlayer,logZ)

    /// Runs an ADF factor graph analysis on a chess outcome dataset with a fixed draw margin; 
    /// If the second parameter is true, then, per year, all the factors are iterated until convergence
    member this.FixedDrawMarginADFAnalyse (param:ChessAnalysisParameters) (iterateYears:bool) (verbose:bool) = 
        /// Compute the internal parameters
        let priorBeliefPlayerMu = param.PriorMuSkill
        let priorBeliefPlayerVariance = param.PriorSigmaSkill * param.PriorSigmaSkill 
        let betaSquared = param.Beta * param.Beta
        let tauSquared = param.TauSkill * param.TauSkill
        
        /// Some useful output
        if verbose then printfn "[Building the factor graph]"
        
        /// The list of factors
        let factorList = FactorList ()
        
        /// The maximal accuracy that we require for convergence
        let delta = param.Delta
        
        /// Allocate the dictionary of all relevant variables
        let varBag = DistributionBag<_> (Gaussian<ELOPoints> (0.0<_>, 0.0<_>))
        
        /// Allocate new messages
        let GF = new GaussianFactors<ELOPoints> ()
        
        /// The dictionary of player skills per year per player
        let playerSkillVarIdxDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of dynamics factors
        let skillDynamicsFDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of prior factors
        let skillPriorFDict = Dictionary<_,_> (HashIdentity.Structural)
        
        /// Allocate all the variables of interest (per year, per playerId); their dynamics and their priors
        if verbose then 
            printfn "[Allocating the skill variables (%d variables)]" (this.PlayerIds |> Seq.fold (fun acc id -> let (s,e) = this.PlayerStartEndYear id in acc + (e-s+1)) 0)
        this.PlayerIds |> Seq.iter (fun id -> 
            let (s,e) = this.PlayerStartEndYear id
            for year = s to e do 
                playerSkillVarIdxDict.Add ((year,id),varBag.NewDistribution ())
                if year > s then 
                    skillDynamicsFDict.Add ((year,id),GF.GaussianLikelihoodFactor tauSquared (playerSkillVarIdxDict.[(year,id)],varBag) (playerSkillVarIdxDict.[(year-1,id)],varBag) |> factorList.AddFactor)
                else
                    skillPriorFDict.Add (id,GF.GaussianPriorFactor (priorBeliefPlayerMu,priorBeliefPlayerVariance) (playerSkillVarIdxDict.[(year,id)],varBag) |> factorList.AddFactor)
            done
        )
        
        /// Convert the probability of draw into an epsilon
        let epsilon = -sqrt (2.0 * betaSquared) * PhiInverse ((1.0 - param.DrawProbability) / 2.0)
        
        /// At first, send the skill priors of all players to the player variable nodes
        let priorSchedule = ScheduleSeq (skillPriorFDict |> Seq.map (fun kvp -> ScheduleStep (kvp.Value,0)))
        
        /// Create a dictionary of matches for each year
        if verbose then printfn "[Grouping the matches by year]"
        let matchesByYear = Dictionary<_,_> ()
        Matches |> Array.iter (fun mtch -> 
            if not (matchesByYear.ContainsKey mtch.Year) then 
                matchesByYear.Add (mtch.Year, new ResizeArray<_> ())
            (matchesByYear.[mtch.Year]).Add (mtch)
        )
        let minYear,maxYear = matchesByYear |> Seq.fold (fun (mn,mx) kvp -> (min mn kvp.Key, max mx kvp.Key)) (Int32.MaxValue,Int32.MinValue)
        
        /// Builds the schedule recursively (forward schedule before recursion and backwards schedule after recursion)
        if verbose then printfn "[Building the schedule year-by-year]"
        let rec BuildSchedule year = 
            /// First, build the schedule where all the dynamics factor send all the message forward in time
            let forwardDynamicsSchedule = 
                ScheduleSeq (skillDynamicsFDict 
                    |> Seq.choose (fun kvp -> let (y,_) = kvp.Key in if y = year then Some (kvp.Value) else None) 
                    |> Seq.map (fun update -> ScheduleStep (update, 0)) 
                    |> Seq.toArray 
                    |> Array.toSeq
                )
            
            /// Now build a loop schedule over all matches
            let dataScheduleSteps = new ResizeArray<_> ()
            
            /// Check that there is at least one match per year
            if matchesByYear.ContainsKey year then 
                matchesByYear.[year] |> Seq.iter (fun mtch -> 
                    let blackPerformance = varBag.NewDistribution ()
                    let whitePerformance = varBag.NewDistribution ()
                    let performanceDiff = varBag.NewDistribution ()
                    let likelBlackF = GF.GaussianLikelihoodFactor betaSquared (blackPerformance,varBag) (playerSkillVarIdxDict.[(year,mtch.BlackPlayerId)],varBag) |> factorList.AddFactor
                    let likelWhiteF = GF.GaussianLikelihoodFactor betaSquared (whitePerformance,varBag) (playerSkillVarIdxDict.[(year,mtch.WhitePlayerId)],varBag) |> factorList.AddFactor
                    let diffF = 
                        match mtch.Outcome with
                        | MatchOutcome.BlackWins    -> GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff,varBag) (blackPerformance,varBag) (whitePerformance,varBag) |> factorList.AddFactor
                        | MatchOutcome.WhiteWins    -> GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff,varBag) (whitePerformance,varBag) (blackPerformance,varBag) |> factorList.AddFactor
                        | MatchOutcome.Draw         -> GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff,varBag) (blackPerformance,varBag) (whitePerformance,varBag) |> factorList.AddFactor
                        | _                         -> failwith "Invalid match outcome"
                        
                    let matchF = 
                        match mtch.Outcome with
                        | MatchOutcome.BlackWins    -> GF.GaussianGreaterThanFactor epsilon (performanceDiff,varBag) |> factorList.AddFactor
                        | MatchOutcome.WhiteWins    -> GF.GaussianGreaterThanFactor epsilon (performanceDiff,varBag) |> factorList.AddFactor
                        | MatchOutcome.Draw         -> GF.GaussianWithinFactor epsilon (performanceDiff,varBag) |> factorList.AddFactor
                        | _                         -> failwith "Invalid match outcome"
                    dataScheduleSteps.Add (ScheduleStep (likelBlackF,0))
                    dataScheduleSteps.Add (ScheduleStep (likelWhiteF,0))
                    dataScheduleSteps.Add (ScheduleStep (diffF,0))
                    dataScheduleSteps.Add (ScheduleStep (matchF,0))
                    dataScheduleSteps.Add (ScheduleStep (diffF,1))
                    dataScheduleSteps.Add (ScheduleStep (diffF,2))
                    dataScheduleSteps.Add (ScheduleStep (likelBlackF,1))
                    dataScheduleSteps.Add (ScheduleStep (likelWhiteF,1))
                )
            let dataSchedule = 
                if iterateYears then
                    ScheduleLoop (ScheduleSeq (dataScheduleSteps |> Seq.toList |> List.toSeq), delta)
                else
                    ScheduleSeq (dataScheduleSteps |> Seq.toList |> List.toSeq)
            
            if verbose then printfn "\tFinished building schedule for year %d." year
            
            /// Get the schedule for the next years
            let nextYearSchedule = if (year < maxYear) then BuildSchedule (year+1) else ScheduleSeq (Seq.empty)

            /// Now connect all the schedules and connect them
            ScheduleSeq ([| forwardDynamicsSchedule; dataSchedule; nextYearSchedule |] |> Array.toSeq)
            
        /// Build the full schedule ...
        let fullSchedule = ScheduleSeq ([| priorSchedule ;BuildSchedule minYear |] |> Array.toSeq)
        
        /// ... and run it
        if verbose then printfn "[Starting to run the schedule]"
        let delta = Run fullSchedule (if verbose then 1 else -1)
        if verbose then printfn "[Finished schedule with precision = %16.15f]" (float delta)
        
        /// Build the list of skill per player 
        if verbose then printfn "[Building the player skill result data structure]"
        let playerSkillsPerPlayer = Dictionary<_,_> ()
        playerSkillVarIdxDict |> Seq.iter (fun kvp -> 
            let (year,id) = kvp.Key
            let playerName = this.PlayerName id
            if not (playerSkillsPerPlayer.ContainsKey playerName) then
                playerSkillsPerPlayer.Add (playerName, new SortedList<_,_> ())
            (playerSkillsPerPlayer.[playerName]).Add (year, varBag.[kvp.Value])
        )
        if verbose then printfn "[Finished the analysis]"
        
        playerSkillsPerPlayer

    /// Runs an forward-backward factor graph analysis on a chess outcome dataset with a fixed draw margin (but using the positivity factor only)
    member this.FixedDrawMarginAnalyse2 (param:ChessAnalysisParameters) (verbose:bool) = 
        /// Compute the internal parameters
        let priorBeliefPlayerMu = param.PriorMuSkill
        let priorBeliefPlayerVariance = param.PriorSigmaSkill * param.PriorSigmaSkill 
        let betaSquared = param.Beta * param.Beta
        let tauSquared = param.TauSkill * param.TauSkill
        
        /// Some useful output
        if verbose then printfn "[Building the factor graph]"
        
        /// The list of factors
        let factorList = FactorList ()
        
        /// The maximal accuracy that we require for convergence
        let delta = param.Delta
        
        /// Allocate the dictionary of all relevant variables
        let varBag = DistributionBag<_> (Gaussian<ELOPoints> (0.0<_>, 0.0<_>))
        
        /// Allocate new messages
        let GF = new GaussianFactors<ELOPoints> ()
                
        /// The dictionary of player skills per year per player
        let playerSkillVarIdxDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of dynamics factors
        let skillDynamicsFDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of prior factors
        let skillPriorFDict = Dictionary<_,_> (HashIdentity.Structural)
        
        /// Allocate all the variables of interest (per year, per playerId); their dynamics and their priors
        if verbose then 
            printfn "[Allocating the skill variables (%d variables)]" (this.PlayerIds |> Seq.fold (fun acc id -> let (s,e) = this.PlayerStartEndYear id in acc + (e-s+1)) 0)
        this.PlayerIds |> Seq.iter (fun id -> 
            let (s,e) = this.PlayerStartEndYear id
            for year = s to e do 
                playerSkillVarIdxDict.Add ((year,id),varBag.NewDistribution ())
                if year > s then 
                    skillDynamicsFDict.Add ((year,id),GF.GaussianLikelihoodFactor tauSquared (playerSkillVarIdxDict.[(year,id)],varBag) (playerSkillVarIdxDict.[(year-1,id)],varBag) |> factorList.AddFactor)
                else
                    skillPriorFDict.Add (id,GF.GaussianPriorFactor (priorBeliefPlayerMu,priorBeliefPlayerVariance) (playerSkillVarIdxDict.[(year,id)],varBag) |> factorList.AddFactor)
            done
        )
        
        /// Convert the probability of draw into an epsilon
        let epsilon = -sqrt (2.0 * betaSquared) * PhiInverse ((1.0 - param.DrawProbability) / 2.0)
        printfn "Epsilon used = %f" (float epsilon)
        
        /// At first, send the skill priors of all players to the player variable nodes
        let priorSchedule = ScheduleSeq (skillPriorFDict |> Seq.map (fun kvp -> ScheduleStep (kvp.Value,0)))
        
        /// Create a dictionary of matches for each year
        if verbose then printfn "[Grouping the matches by year]"
        let matchesByYear = Dictionary<_,_> ()
        Matches |> Array.iter (fun mtch -> 
            if not (matchesByYear.ContainsKey mtch.Year) then 
                matchesByYear.Add (mtch.Year, new ResizeArray<_> ())
            (matchesByYear.[mtch.Year]).Add (mtch)
        )
        let minYear,maxYear = matchesByYear |> Seq.fold (fun (mn,mx) kvp -> (min mn kvp.Key, max mx kvp.Key)) (Int32.MaxValue,Int32.MinValue)
        
        /// Builds the schedule recursively (forward schedule before recursion and backwards schedule after recursion)
        if verbose then printfn "[Building the schedule year-by-year]"
        let rec BuildSchedule year = 
            /// First, build the schedule where all the dynamics factor send all the message forward in time
            let forwardDynamicsSchedule = 
                ScheduleSeq (skillDynamicsFDict 
                    |> Seq.choose (fun kvp -> let (y,_) = kvp.Key in if y = year then Some (kvp.Value) else None) 
                    |> Seq.map (fun update -> ScheduleStep (update, 0)) 
                    |> Seq.toArray 
                    |> Array.toSeq
                )
            
            /// Now build a loop schedule over all matches
            let dataScheduleSteps = new ResizeArray<_> ()
            
            /// Check that there is at least one match per year
            if matchesByYear.ContainsKey year then 
                matchesByYear.[year] |> Seq.iter (fun mtch -> 
                    let blackPerformance = varBag.NewDistribution ()
                    let whitePerformance = varBag.NewDistribution ()
                    let likelBlackF = GF.GaussianLikelihoodFactor betaSquared (blackPerformance,varBag) (playerSkillVarIdxDict.[(year,mtch.BlackPlayerId)],varBag) |> factorList.AddFactor
                    let likelWhiteF = GF.GaussianLikelihoodFactor betaSquared (whitePerformance,varBag) (playerSkillVarIdxDict.[(year,mtch.WhitePlayerId)],varBag) |> factorList.AddFactor
                    dataScheduleSteps.Add (ScheduleStep (likelBlackF,0))
                    dataScheduleSteps.Add (ScheduleStep (likelWhiteF,0))
                    do
                        match mtch.Outcome with
                        | MatchOutcome.BlackWins    -> 
                            let performanceDiff = varBag.NewDistribution ()
                            let diffF = GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff,varBag) (blackPerformance,varBag) (whitePerformance,varBag) |> factorList.AddFactor
                            let matchF = GF.GaussianGreaterThanFactor epsilon (performanceDiff,varBag) |> factorList.AddFactor
                            dataScheduleSteps.Add (ScheduleStep (diffF,0))
                            dataScheduleSteps.Add (ScheduleStep (matchF,0))
                            dataScheduleSteps.Add (ScheduleStep (diffF,1))
                            dataScheduleSteps.Add (ScheduleStep (diffF,2))
                        | MatchOutcome.WhiteWins    -> 
                            let performanceDiff = varBag.NewDistribution ()
                            let diffF = GF.GaussianWeightedSumFactor (-1.0) (1.0) (performanceDiff,varBag) (blackPerformance,varBag) (whitePerformance,varBag) |> factorList.AddFactor
                            let matchF = GF.GaussianGreaterThanFactor epsilon (performanceDiff,varBag) |> factorList.AddFactor
                            dataScheduleSteps.Add (ScheduleStep (diffF,0))
                            dataScheduleSteps.Add (ScheduleStep (matchF,0))
                            dataScheduleSteps.Add (ScheduleStep (diffF,1))
                            dataScheduleSteps.Add (ScheduleStep (diffF,2))
                        | MatchOutcome.Draw         -> 
                            let performanceDiff1 = varBag.NewDistribution ()
                            let diff1F = GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff1,varBag) (blackPerformance,varBag) (whitePerformance,varBag) |> factorList.AddFactor
                            let match1F = GF.GaussianGreaterThanFactor (-epsilon) (performanceDiff1,varBag) |> factorList.AddFactor
                            dataScheduleSteps.Add (ScheduleStep (diff1F,0))
                            dataScheduleSteps.Add (ScheduleStep (match1F,0))
                            dataScheduleSteps.Add (ScheduleStep (diff1F,1))
                            dataScheduleSteps.Add (ScheduleStep (diff1F,2))
                            
                            let performanceDiff2 = varBag.NewDistribution ()
                            let diff2F = GF.GaussianWeightedSumFactor (-1.0) (1.0) (performanceDiff2,varBag) (blackPerformance,varBag) (whitePerformance,varBag) |> factorList.AddFactor
                            let match2F = GF.GaussianGreaterThanFactor (-epsilon) (performanceDiff2,varBag) |> factorList.AddFactor
                            dataScheduleSteps.Add (ScheduleStep (diff2F,0))
                            dataScheduleSteps.Add (ScheduleStep (match2F,0))
                            dataScheduleSteps.Add (ScheduleStep (diff2F,1))
                            dataScheduleSteps.Add (ScheduleStep (diff2F,2))
                        | _                         -> failwith "Invalid match outcome"
                    dataScheduleSteps.Add (ScheduleStep (likelBlackF,1))
                    dataScheduleSteps.Add (ScheduleStep (likelWhiteF,1))
                )
            let dataSchedule = ScheduleLoop (ScheduleSeq (dataScheduleSteps |> Seq.toList |> List.toSeq), delta)
            
            if verbose then printfn "\tFinished building schedule for year %d." year
            
            /// Get the schedule for the next years
            let nextYearSchedule = if (year < maxYear) then BuildSchedule (year+1) else ScheduleSeq (Seq.empty)

            /// Lastly, build the schedule where all the dynamics factor send all the message backward in time
            let backwardDynamicsSchedule = 
                ScheduleSeq (skillDynamicsFDict 
                    |> Seq.choose (fun kvp -> let (y,_) = kvp.Key in if y = year then Some (kvp.Value) else None) 
                    |> Seq.map (fun update -> ScheduleStep (update, 1)) 
                    |> Seq.toArray 
                    |> Array.toSeq
                )

            /// Now connect all the schedules and connect them
            ScheduleSeq ([| forwardDynamicsSchedule; dataSchedule; nextYearSchedule; dataSchedule; backwardDynamicsSchedule |] |> Array.toSeq)
            
        /// Build the full schedule ...
        let fullSchedule = ScheduleLoop (ScheduleSeq ([| priorSchedule ;BuildSchedule minYear |] |> Array.toSeq), delta)
        
        /// ... and run it
        if verbose then printfn "[Starting to run the schedule]"
        let delta = Run fullSchedule (if verbose then 1 else -1)
        if verbose then printfn "[Finished schedule with precision = %16.15f]" (float delta)
        
        /// Output the normalisation constant
        let logZ = factorList.LogNormalisation
        if verbose then printfn "[Log Normalisation = %16.15f]" logZ
        
        /// Build the list of skill per player 
        if verbose then printfn "[Building the player skill result data structure]"
        let playerSkillsPerPlayer = Dictionary<_,_> ()
        playerSkillVarIdxDict |> Seq.iter (fun kvp -> 
            let (year,id) = kvp.Key
            let playerName = this.PlayerName id
            if not (playerSkillsPerPlayer.ContainsKey playerName) then
                playerSkillsPerPlayer.Add (playerName, new SortedList<_,_> ())
            (playerSkillsPerPlayer.[playerName]).Add (year, varBag.[kvp.Value])
        )
        if verbose then printfn "[Finished the analysis]"
        
        (playerSkillsPerPlayer,logZ)
        
    /// Runs a full forward-backward factor graph analysis on a chess outcome dataset with a variable draw margin
    member this.VariableDrawMarginAnalyse (param:ChessAnalysisParameters) (verbose:bool) = 
        /// Compute the internal parameters
        let priorBeliefPlayerMu = param.PriorMuSkill
        let priorBeliefPlayerVariance = param.PriorSigmaSkill * param.PriorSigmaSkill 
        let betaSquared = param.Beta * param.Beta
        let tauSquared = param.TauSkill * param.TauSkill
        let priorBeliefDMVariance = param.PriorSigmaDrawMargin * param.PriorSigmaDrawMargin
        let priorBeliefDMMu = param.PriorMuDrawMargin
        let tauDMSquared = param.TauDrawMargin * param.TauDrawMargin
        
        /// Some useful output
        if verbose then printfn "[Building the factor graph]"
        
        /// The list of factors
        let factorList = FactorList ()
        
        /// The maximal accuracy that we require for convergence
        let delta = param.Delta
        
        /// Allocate the dictionary of all relevant variables
        let varBag = DistributionBag<_> (Gaussian<ELOPoints> (0.0<_>, 0.0<_>))
        
        /// Allocate new messages
        let GF = new GaussianFactors<ELOPoints> ()
                
        /// The dictionary of player skills and draw margins per year per player
        let playerSkillDrawMarginDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of skill dynamics factors
        let skillDynamicsFDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of skill prior factors
        let skillPriorFDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of draw margin dynamics factors
        let drawMarginDynamicsFDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of draw margin prior factors
        let drawMarginPriorFDict = Dictionary<_,_> (HashIdentity.Structural)
        /// The dictionary of draw margin positivity factors
        let drawMarginPositivityFDict = Dictionary<_,_> (HashIdentity.Structural)
        
        /// Allocate all the variables of interest (per year, per playerId); their dynamics and their priors
        if verbose then 
            printfn "[Allocating the skill variables (%d variables)]" (this.PlayerIds |> Seq.fold (fun acc id -> let (s,e) = this.PlayerStartEndYear id in acc + 2*(e-s+1)) 0)
        this.PlayerIds |> Seq.iter (fun id -> 
            let (s,e) = this.PlayerStartEndYear id
            for year = s to e do 
                playerSkillDrawMarginDict.Add ((year,id),SkillDrawMargin (varBag.NewDistribution (),varBag.NewDistribution ()))
                drawMarginPositivityFDict.Add ((year,id), GF.GaussianGreaterThanFactor 0.0<_> (playerSkillDrawMarginDict.[(year,id)].DrawMarginIdx,varBag) |> factorList.AddFactor)
                if year > s then 
                    skillDynamicsFDict.Add ((year,id),GF.GaussianLikelihoodFactor tauSquared (playerSkillDrawMarginDict.[(year,id)].SkillIdx,varBag) (playerSkillDrawMarginDict.[(year-1,id)].SkillIdx,varBag) |> factorList.AddFactor)
                    drawMarginDynamicsFDict.Add ((year,id),GF.GaussianLikelihoodFactor tauDMSquared (playerSkillDrawMarginDict.[(year,id)].DrawMarginIdx,varBag) (playerSkillDrawMarginDict.[(year-1,id)].DrawMarginIdx,varBag) |> factorList.AddFactor)
                else
                    skillPriorFDict.Add (id,GF.GaussianPriorFactor (priorBeliefPlayerMu,priorBeliefPlayerVariance) (playerSkillDrawMarginDict.[(year,id)].SkillIdx,varBag) |> factorList.AddFactor)
                    drawMarginPriorFDict.Add (id,GF.GaussianPriorFactor (priorBeliefDMMu,priorBeliefDMVariance) (playerSkillDrawMarginDict.[(year,id)].DrawMarginIdx,varBag) |> factorList.AddFactor)
            done
        )
        
        /// At first, send the skill priors of all players to the player variable nodes
        let skillPriorScheduleSteps = skillPriorFDict |> Seq.map (fun kvp -> ScheduleStep (kvp.Value,0))
        let drawMarginPriorScheduleSteps = drawMarginPriorFDict |> Seq.map (fun kvp -> ScheduleStep (kvp.Value,0))
        let priorSchedule = ScheduleSeq (Seq.append drawMarginPriorScheduleSteps skillPriorScheduleSteps)
        
        /// Create a dictionary of matches for each year
        if verbose then printfn "[Grouping the matches by year]"
        let matchesByYear = Dictionary<_,_> ()
        Matches |> Array.iter (fun mtch -> 
            if not (matchesByYear.ContainsKey mtch.Year) then 
                matchesByYear.Add (mtch.Year, new ResizeArray<_> ())
            (matchesByYear.[mtch.Year]).Add (mtch)
        )
        let minYear,maxYear = matchesByYear |> Seq.fold (fun (mn,mx) kvp -> (min mn kvp.Key, max mx kvp.Key)) (Int32.MaxValue,Int32.MinValue)
        
        /// Builds the schedule recursively (forward schedule before recursion and backwards schedule after recursion)
        if verbose then printfn "[Building the schedule year-by-year]"
        let rec BuildSchedule year = 
            /// First, build the schedule where all the dynamics factor send all the message forward in time
            let forwardDynamicsSchedule = 
                let skillForwardScheduleSteps = 
                    skillDynamicsFDict 
                        |> Seq.choose (fun kvp -> let (y,_) = kvp.Key in if y = year then Some (kvp.Value) else None) 
                        |> Seq.map (fun update -> ScheduleStep (update, 0)) 
                        |> Seq.toArray 
                        |> Array.toSeq
                let drawMarginForwardScheduleSteps = 
                    drawMarginDynamicsFDict
                        |> Seq.choose (fun kvp -> let (y,_) = kvp.Key in if y = year then Some (kvp.Value) else None) 
                        |> Seq.map (fun update -> ScheduleStep (update, 0)) 
                        |> Seq.toArray 
                        |> Array.toSeq
                ScheduleSeq (Seq.append drawMarginForwardScheduleSteps skillForwardScheduleSteps)
            
            /// Now build a loop schedule over all matches
            let dataScheduleSteps = new ResizeArray<_> ()
            
            /// Check that there is at least one match per year
            if matchesByYear.ContainsKey year then 
                matchesByYear.[year] |> Seq.iter (fun mtch -> 
                    let blackPerformance = varBag.NewDistribution ()
                    let whitePerformance = varBag.NewDistribution ()
                    let likelBlackF = GF.GaussianLikelihoodFactor betaSquared (blackPerformance,varBag) (playerSkillDrawMarginDict.[(year,mtch.BlackPlayerId)].SkillIdx,varBag) |> factorList.AddFactor
                    let likelWhiteF = GF.GaussianLikelihoodFactor betaSquared (whitePerformance,varBag) (playerSkillDrawMarginDict.[(year,mtch.WhitePlayerId)].SkillIdx,varBag) |> factorList.AddFactor
                    dataScheduleSteps.Add (ScheduleStep (likelBlackF,0))
                    dataScheduleSteps.Add (ScheduleStep (likelWhiteF,0))
                    do 
                        match mtch.Outcome with
                        | MatchOutcome.BlackWins    -> 
                            let performanceWhitePlusDrawMargin = varBag.NewDistribution ()
                            let performanceDiff = varBag.NewDistribution ()
                            let addF = GF.GaussianWeightedSumFactor (1.0) (1.0) (performanceWhitePlusDrawMargin,varBag) (whitePerformance,varBag) (playerSkillDrawMarginDict.[(year,mtch.WhitePlayerId)].DrawMarginIdx,varBag) |> factorList.AddFactor
                            let subF = GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff,varBag) (blackPerformance,varBag) (performanceWhitePlusDrawMargin,varBag) |> factorList.AddFactor
                            let matchF = GF.GaussianGreaterThanFactor 0.0<_> (performanceDiff,varBag) |> factorList.AddFactor
                            dataScheduleSteps.Add (ScheduleStep (addF,0))
                            dataScheduleSteps.Add (ScheduleStep (subF,0))
                            dataScheduleSteps.Add (ScheduleStep (matchF,0))
                            dataScheduleSteps.Add (ScheduleStep (subF,1))
                            dataScheduleSteps.Add (ScheduleStep (subF,2))
                            dataScheduleSteps.Add (ScheduleStep (addF,1))
                            dataScheduleSteps.Add (ScheduleStep (addF,2))
                            dataScheduleSteps.Add (ScheduleStep (drawMarginPositivityFDict.[(year,mtch.WhitePlayerId)],0))
                        | MatchOutcome.WhiteWins    -> 
                            let performanceBlackPlusDrawMargin = varBag.NewDistribution ()
                            let performanceDiff = varBag.NewDistribution ()
                            let addF = GF.GaussianWeightedSumFactor (1.0) (1.0) (performanceBlackPlusDrawMargin,varBag) (blackPerformance,varBag) (playerSkillDrawMarginDict.[(year,mtch.BlackPlayerId)].DrawMarginIdx,varBag) |> factorList.AddFactor
                            let subF = GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiff,varBag) (whitePerformance,varBag) (performanceBlackPlusDrawMargin,varBag) |> factorList.AddFactor
                            let matchF = GF.GaussianGreaterThanFactor 0.0<_> (performanceDiff,varBag) |> factorList.AddFactor
                            dataScheduleSteps.Add (ScheduleStep (addF,0))
                            dataScheduleSteps.Add (ScheduleStep (subF,0))
                            dataScheduleSteps.Add (ScheduleStep (matchF,0))
                            dataScheduleSteps.Add (ScheduleStep (subF,1))
                            dataScheduleSteps.Add (ScheduleStep (subF,2))
                            dataScheduleSteps.Add (ScheduleStep (addF,1))
                            dataScheduleSteps.Add (ScheduleStep (addF,2))
                            dataScheduleSteps.Add (ScheduleStep (drawMarginPositivityFDict.[(year,mtch.BlackPlayerId)],0))
                        | MatchOutcome.Draw         -> 
                            let performanceWhitePlusDrawMargin = varBag.NewDistribution ()
                            let performanceDiffWhite = varBag.NewDistribution ()
                            let add1F = GF.GaussianWeightedSumFactor (1.0) (1.0) (performanceWhitePlusDrawMargin,varBag) (whitePerformance,varBag) (playerSkillDrawMarginDict.[(year,mtch.WhitePlayerId)].DrawMarginIdx,varBag) |> factorList.AddFactor
                            let sub1F = GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiffWhite,varBag) (performanceWhitePlusDrawMargin,varBag) (blackPerformance,varBag) |> factorList.AddFactor
                            let match1F = GF.GaussianGreaterThanFactor 0.0<_> (performanceDiffWhite,varBag) |> factorList.AddFactor
                            dataScheduleSteps.Add (ScheduleStep (add1F,0))
                            dataScheduleSteps.Add (ScheduleStep (sub1F,0))
                            dataScheduleSteps.Add (ScheduleStep (match1F,0))
                            dataScheduleSteps.Add (ScheduleStep (sub1F,1))
                            dataScheduleSteps.Add (ScheduleStep (sub1F,2))
                            dataScheduleSteps.Add (ScheduleStep (add1F,1))
                            dataScheduleSteps.Add (ScheduleStep (add1F,2))
                            dataScheduleSteps.Add (ScheduleStep (drawMarginPositivityFDict.[(year,mtch.WhitePlayerId)],0))

                            let performanceBlackPlusDrawMargin = varBag.NewDistribution ()
                            let performanceDiffBlack = varBag.NewDistribution ()
                            let add2F = GF.GaussianWeightedSumFactor (1.0) (1.0) (performanceBlackPlusDrawMargin,varBag) (blackPerformance,varBag) (playerSkillDrawMarginDict.[(year,mtch.BlackPlayerId)].DrawMarginIdx,varBag) |> factorList.AddFactor
                            let sub2F = GF.GaussianWeightedSumFactor (1.0) (-1.0) (performanceDiffBlack,varBag) (performanceBlackPlusDrawMargin,varBag) (whitePerformance,varBag) |> factorList.AddFactor
                            let match2F = GF.GaussianGreaterThanFactor 0.0<_> (performanceDiffBlack,varBag) |> factorList.AddFactor
                            dataScheduleSteps.Add (ScheduleStep (add2F,0))
                            dataScheduleSteps.Add (ScheduleStep (sub2F,0))
                            dataScheduleSteps.Add (ScheduleStep (match2F,0))
                            dataScheduleSteps.Add (ScheduleStep (sub2F,1))
                            dataScheduleSteps.Add (ScheduleStep (sub2F,2))
                            dataScheduleSteps.Add (ScheduleStep (add2F,1))
                            dataScheduleSteps.Add (ScheduleStep (add2F,2))
                            dataScheduleSteps.Add (ScheduleStep (drawMarginPositivityFDict.[(year,mtch.BlackPlayerId)],0))
                        | _                         -> 
                            failwith "Invalid match outcome"
                            
                    dataScheduleSteps.Add (ScheduleStep (likelBlackF,1))
                    dataScheduleSteps.Add (ScheduleStep (likelWhiteF,1))
                ) 
            let dataSchedule = ScheduleLoop (ScheduleSeq (dataScheduleSteps |> Seq.toList |> List.toSeq), delta)
            
            if verbose then printfn "\tFinished building schedule for year %d." year
            
            /// Get the schedule for the next years
            let nextYearSchedule = if (year < maxYear) then BuildSchedule (year+1) else ScheduleSeq (Seq.empty)

            /// Lastly, build the schedule where all the dynamics factor send all the message backward in time
            let backwardDynamicsSchedule = 
                let skillBackwardScheduleSteps = 
                    skillDynamicsFDict 
                        |> Seq.choose (fun kvp -> let (y,_) = kvp.Key in if y = year then Some (kvp.Value) else None) 
                        |> Seq.map (fun update -> ScheduleStep (update, 1)) 
                        |> Seq.toArray 
                        |> Array.toSeq
                let drawMarginBackwardScheduleSteps = 
                    drawMarginDynamicsFDict
                        |> Seq.choose (fun kvp -> let (y,_) = kvp.Key in if y = year then Some (kvp.Value) else None) 
                        |> Seq.map (fun update -> ScheduleStep (update, 1)) 
                        |> Seq.toArray 
                        |> Array.toSeq
                ScheduleSeq (Seq.append drawMarginBackwardScheduleSteps skillBackwardScheduleSteps)    

            /// Now connect all the schedules and connect them
            ScheduleSeq ([| forwardDynamicsSchedule; dataSchedule; nextYearSchedule; dataSchedule; backwardDynamicsSchedule |] |> Array.toSeq)
            
        /// Build the full schedule ...
        let fullSchedule = ScheduleLoop (ScheduleSeq ([| priorSchedule ;BuildSchedule minYear |] |> Array.toSeq), delta)
        
        /// ... and run it
        if verbose then printfn "[Starting to run the schedule]"
        let delta = Run fullSchedule (if verbose then 1 else -1)
        if verbose then printfn "[Finished schedule with precision = %16.15f]" (float delta)
        
        /// Output the normalisation constant
        let logZ = factorList.LogNormalisation
        if verbose then printfn "[Log Normalisation = %16.15f]" logZ
        
        /// Build the list of skill per player 
        if verbose then printfn "[Building the player skill result data structure]"
        let resData = Dictionary<_,_> ()
        playerSkillDrawMarginDict |> Seq.iter (fun kvp -> 
            let (year,id) = kvp.Key
            let playerName = this.PlayerName id
            if not (resData.ContainsKey playerName) then
                resData.Add (playerName, new SortedList<_,_> ())
            (resData.[playerName]).Add (year, (varBag.[kvp.Value.SkillIdx],varBag.[kvp.Value.DrawMarginIdx]))
        )
        if verbose then printfn "[Finished the analysis]"
        
        (resData,logZ)

module Seq = 
    open System.Collections.Generic

    /// A lazy implementation of groupBy which does NOT read the whole sequence in memory first
    let chunkBy f (s:seq<_>) = 
        seq {
            use ie = s.GetEnumerator ()
            let notAtEnd = ref (ie.MoveNext ())
            while !notAtEnd do
                let key = f ie.Current
                let group = List<_> (3)
                while !notAtEnd && (f ie.Current) = key do 
                    group.Add (ie.Current)
                    notAtEnd := ie.MoveNext ()
                yield (key,(group :> seq<_>))   
        }
    