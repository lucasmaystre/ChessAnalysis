//--------------------------------------------------------------
// Program.fs           Chess analysis for the NIPS paper
//
// 2007/2008 written by Ralf Herbrich
//
// Copyright (c) 2002-2011 Microsoft Research Ltd.
//
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
//
//
//--------------------------------------------------------------

open System
open System.IO
open System.Collections.Generic
open MSRC.Inference.Distributions
open MSRC.Inference.Distributions.Gaussians
open MSRC.Chess
open MSRC.Arg

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
/// The path to the output file.
let outputPath = ref "output.csv"
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
        ArgInfo("-no-safe",Arg.Set notSave,                                    "Does not save results predictions (default: off)")
        ArgInfo("-q",      Arg.Clear verbose,                                  "Verbosity level off; only works on single runs (default: on)")
        ArgInfo("-N",      Arg.Int (fun n -> noGames := Some (n)),             "First N games only (default: ALL)")
        ArgInfo("-delta",  Arg.Float (fun aDelta -> delta := aDelta),          "Maximum deviation in any marginal for convergence (default: 0.01)")
        ArgInfo("-muS",    Arg.Float (fun aMu -> muSkill := aMu),              "Mean of prior skill belief (default: 1200)")
        ArgInfo("-sigmaS", Arg.Float (fun aSigma -> sigmaSkill := aSigma),     "Standard deviation of prior skill belief (default: 400)")
        ArgInfo("-muD",    Arg.Float (fun aMu -> muDrawMargin := aMu),         "Mean of prior draw margin belief (default: 300)")
        ArgInfo("-sigmaD", Arg.Float (fun aSigma -> sigmaDrawMargin := aSigma),"Standard deviation of prior draw margin belief (default: 100)")
        ArgInfo("-beta",   Arg.Float (fun aBeta -> beta := aBeta),             "Standard deviation of performance distr. (default: 600)")
        ArgInfo("-tauS",   Arg.Float (fun aTau -> tauSkill := aTau),           "Standard deviation of skill dynamics distr. (default: 40)")
        ArgInfo("-tauD",   Arg.Float (fun aTau -> tauDrawMargin := aTau),      "Standard deviation of draw margin dynamics distr. (default: 10)")
        ArgInfo("-out",    Arg.String (fun aPath -> outputPath := aPath),      "Path of CSV output (default: 'output.csv')")
        ArgInfo("-mf",     Arg.Unit (fun () -> model := FixedDrawMargin),      "Fixed draw margin (default)")
        ArgInfo("-mf2",    Arg.Unit (fun () -> model := FixedDrawMargin2),     "Fixed draw margin with two factors for draw")
        ArgInfo("-maf",    Arg.Unit (fun () -> model := FixedDrawMarginADFOverYears),  "Fixed draw margin with ADF (iterate per year)")
        ArgInfo("-maf2",   Arg.Unit (fun () -> model := FixedDrawMarginADF),   "Fixed draw margin with pure ADF")
        ArgInfo("-mv",     Arg.Unit (fun () -> model := VariableDrawMargin),   "Variable draw margin")
        ArgInfo("-as",     Arg.Unit (fun () -> analysis := SingleRun),         "Single run (default)")
        ArgInfo("-am",     Arg.Unit (fun () -> analysis := ModelSelection),    "Model selection")
    ]

/// Saves the result of a fixed draw margin analysis to a CSV file
let SaveFixedDrawMarginResults (results:Dictionary<string,SortedList<int,Gaussian<_>>>) =
    if !notSave then
        ()
    else
        let wr = new System.IO.StreamWriter(!outputPath)
        wr.WriteLine("playerName,year,mu,sigma")
        results |> Seq.iter (fun kvp ->
            let playerName = kvp.Key
            kvp.Value |> Seq.iter (fun kvp ->
                let year = string kvp.Key
                let mu = string kvp.Value.Mu
                let sigma = string kvp.Value.Sigma
                [playerName; year; mu; sigma] |> String.concat(",") |> wr.WriteLine
            )
        )
        wr.Close()

/// Saves the result of a variable draw margin analysis to a CSV file
let SaveVariableDrawMarginResults (results:Dictionary<string,SortedList<int,Gaussian<_>*Gaussian<_>>>) =
    if !notSave then
        ()
    else
        let wr = new System.IO.StreamWriter(!outputPath)
        wr.WriteLine("playerName,year,skillMu,skillSigma,drawMarginMu,drawMarginSigma")
        results |> Seq.iter (fun kvp ->
            let playerName = kvp.Key
            kvp.Value |> Seq.iter (fun kvp ->
                let year = string kvp.Key
                let skillMu = string (fst kvp.Value).Mu
                let skillSigma = string (fst kvp.Value).Sigma
                let drawMarginMu = string (snd kvp.Value).Mu
                let drawMarginSigma = string (snd kvp.Value).Sigma
                [playerName; year; skillMu; skillSigma; drawMarginMu; drawMarginSigma] |> String.concat(",") |> wr.WriteLine
            )
        )
        wr.Close()

do
    /// Some useful information on the screen
    printfn "Chess Analysis program v 1.0"
    printfn "2007 written by Ralf Herbrich"
    printfn "Microsoft Research Ltd."
    printfn ""

    /// Parse the arguments
    ArgParser.Parse (argspec,(fun fileName -> csvFile := fileName),"USAGE: chess [params] [<csv-file>]")

    /// Allocate the chess dataset
    let cds = ChessDataset ()

    printfn "[Reading the dataset]"
    cds.ReadFromCSVFile (!csvFile,!noGames)

    match !analysis with
    | SingleRun ->
        /// Output some useful info and get the empirical draw probability
        printfn "[Single run on the dataset using model '%s']" (ModelName())
        let p = float (cds.MatchSequence |> Seq.filter (fun mtch -> mtch.Outcome = MatchOutcome.Draw) |> Seq.length) / float (cds.MatchSequence |> Seq.length)
        let chessAnalysisParameters = ChessAnalysisParameters (PriorMuSkill = !muSkill * 1.0<ELOPoints>, PriorSigmaSkill = !sigmaSkill * 1.0<ELOPoints>, PriorMuDrawMargin = !muDrawMargin * 1.0<ELOPoints>, PriorSigmaDrawMargin = !sigmaDrawMargin * 1.0<ELOPoints>, Beta = !beta * 1.0<ELOPoints>, TauSkill = !tauSkill * 1.0<ELOPoints>, TauDrawMargin = !tauDrawMargin * 1.0<ELOPoints>, DrawProbability = p)
        printfn "[Parameters used]"
        printfn "%O" chessAnalysisParameters

        match !model with
        | FixedDrawMargin ->
            let (results,logZ) = cds.FixedDrawMarginAnalyse chessAnalysisParameters !verbose
            SaveFixedDrawMarginResults results
        | FixedDrawMargin2 ->
            let (results,logZ) = cds.FixedDrawMarginAnalyse2 chessAnalysisParameters !verbose
            SaveFixedDrawMarginResults results
        | FixedDrawMarginADFOverYears ->
            let results = cds.FixedDrawMarginADFAnalyse chessAnalysisParameters true !verbose
            SaveFixedDrawMarginResults results
        | FixedDrawMarginADF ->
            let results = cds.FixedDrawMarginADFAnalyse chessAnalysisParameters false !verbose
            SaveFixedDrawMarginResults results
        | VariableDrawMargin ->
            let (results,logZ) = cds.VariableDrawMarginAnalyse chessAnalysisParameters !verbose
            SaveVariableDrawMarginResults results
            ()
            // SaveFixedDrawMarginResults results
    | ModelSelection ->
        /// Output some useful info and get the empirical draw probability
        printfn "[Model selection run on the dataset using model '%s']" (ModelName())
        let p = float (cds.MatchSequence |> Seq.filter (fun mtch -> mtch.Outcome = MatchOutcome.Draw) |> Seq.length) / float (cds.MatchSequence |> Seq.length)

        match !model with
        | FixedDrawMargin ->
            let p = float (cds.MatchSequence |> Seq.filter (fun mtch -> mtch.Outcome = MatchOutcome.Draw) |> Seq.length) / float (cds.MatchSequence |> Seq.length)
            /// Iterate over the range of all beta/tau values
            let betaTauRange = [| for betaF in 2 .. 2 .. 20 do for tauF in 5 .. 5 .. 50 -> ((float betaF)/10.0,(float tauF)/100.0) |]
            /// Map the range to the log normalisation constant
            betaTauRange |> Array.iteri (fun i (betaF,tauF) ->
                let chessAnalysisParameters = ChessAnalysisParameters (PriorMuSkill = !muSkill * 1.0<ELOPoints>, PriorSigmaSkill = !sigmaSkill * 1.0<ELOPoints>, Beta = !sigmaSkill*betaF * 1.0<ELOPoints>, TauSkill = !sigmaSkill*tauF * 1.0<ELOPoints>, DrawProbability = p)
                let (results,logZ) = cds.FixedDrawMarginAnalyse chessAnalysisParameters false
                printf "%f,%f,%f\n" betaF tauF logZ

                SaveFixedDrawMarginResults results
            )
        | FixedDrawMargin2 ->
            printfn "Not implemented!"
        | FixedDrawMarginADFOverYears ->
            printfn "Not implemented!"
        | FixedDrawMarginADF ->
            printfn "Not implemented!"
        | VariableDrawMargin ->
            /// Build the result schema
            let p = float (cds.MatchSequence |> Seq.filter (fun mtch -> mtch.Outcome = MatchOutcome.Draw) |> Seq.length) / float (cds.MatchSequence |> Seq.length)
            /// Iterate over the range of all beta/tau values
            let muDrawMarginTauDrawMarginRange = [| for muDrawMarginF in 5 .. 1 .. 10 do for tauDrawMarginF in 5 .. 5 .. 20 -> ((float muDrawMarginF)/10.0,(float tauDrawMarginF)/100.0) |]
            /// Map the range to the log normalisation constant
            muDrawMarginTauDrawMarginRange |> Array.iteri (fun i (muDrawMarginF,tauDrawMarginF) ->
                let sigmaDrawMargin = !beta / 6.0
                let chessAnalysisParameters = ChessAnalysisParameters (PriorMuSkill = !muSkill * 1.0<ELOPoints>, PriorSigmaSkill = !sigmaSkill * 1.0<ELOPoints>, PriorMuDrawMargin = !beta*muDrawMarginF * 1.0<ELOPoints>, PriorSigmaDrawMargin = sigmaDrawMargin * 1.0<ELOPoints>, Beta = !beta * 1.0<ELOPoints>, TauSkill = !tauSkill * 1.0<ELOPoints>, TauDrawMargin = tauDrawMarginF*sigmaDrawMargin * 1.0<ELOPoints>, DrawProbability = p)
                let (results,logZ) = cds.VariableDrawMarginAnalyse chessAnalysisParameters false
                printf "%f,%f,%f\n" muDrawMarginF tauDrawMarginF logZ

                /// Output the dataset to a CSV file
                SaveVariableDrawMarginResults results
            )

