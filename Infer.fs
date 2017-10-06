//--------------------------------------------------------------
// Infer.fs           A small factor graph inference library skeleton
//
// 2007/2008 written by Ralf Herbrich
//
// Copyright (c) 2002-2011 Microsoft Research Ltd.
//
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
//
//--------------------------------------------------------------

namespace MSRC.Inference

open System

module Factors =
    /// An IFactor represents all the functionality exposed by a factor
    type IFactor<[<Measure>] 'a> =
        {
            /// Update the message and marginal of the ith variable that the factor is connected to
            UpdateMessage : int -> float</'a>
            /// Returns the log-normalisation constant of that factor
            LogNormalisation : unit -> float
            /// Returns the number of messages that the factor has
            NumberOfMessages : int
            /// Resets the marginal of the varaibles a factor is connected to
            ResetMarginals : unit -> unit
            /// Sends the ith message to the marginal and returns the log-normalisation constant
            SendMessage : int -> float
        }

module Collections =
    open System.Collections.Generic
    open Factors

    /// A distribution bag is a variable sized array of strongly typed objects
    type DistributionBag<'a> (prior:'a) =
        /// Internally, the distribution bag is a resize-able array
        let mutable bag = new ResizeArray<'a> ()

        /// Gives the index to a new distribution
        member this.NewDistribution () =
            let id = bag.Count
            bag.Add (prior)
            id

        /// Returns the prior used in the distribution bag
        member this.Prior = prior

        /// The current number of variables
        member this.NoVariables = bag.Count

        /// Clear the entire bag of distributions
        member this.Reset () =
            bag <- new ResizeArray<'a> ()

        /// Indexers for the distribution bag
        member this.Item
            with get idx = bag.[idx]
            and set idx v = bag.[idx] <- v

    /// A factor list is a collection of factors used. This is important in order to compute
    /// the correction factor for the normalisation constant
    type FactorList<[<Measure>] 'a>() =
        /// The list of factors
        let fList = new ResizeArray<IFactor<'a>> ()

        /// Adds a factor to the list of factors and returns it
        member this.AddFactor (f:IFactor<'a>) =
            fList.Add f
            f

        /// Computes the log normalisation constant.
        member this.LogNormalisation =
            /// Reset the marginals
            fList |> Seq.iter (fun f -> f.ResetMarginals())

            /// Reconstruct the marginal but count the normalisation factors
            let mutable sumLogZ = 0.0
            let n = fList.Count-1
            for i = 0 to n do
                let f = fList.[i]
                for j = 0 to (f.NumberOfMessages-1) do
                    sumLogZ <- sumLogZ + f.SendMessage j
                done
            done

            /// Get the product of S_i
            let sumLogS = fList |> Seq.fold (fun acc f -> acc + (f.LogNormalisation ())) 0.0

            sumLogZ + sumLogS

module Schedule =
    open Factors
    open Collections

    /// A schedule of computation in a factor graph (i.e., a series of update functions and variables that should be updated)
    type Schedule<[<Measure>] 'a> =
    | ScheduleStep of (IFactor<'a> * int)
    | ScheduleSeq of Schedule<'a> seq
    | ScheduleLoop of Schedule<'a> * float</'a>

    /// Creates a schedule sequence for any data structure that implements IEnumerable
    let Schedules (xa:#seq<Schedule<_>>) = ScheduleSeq (xa :> seq<Schedule<_>>)

    /// Runs a schedule with display up to level i
    let Run schedule maxDepth =
        let MakeStr n c = String (Array.init n (fun _ -> c))
        let rec RunInternal depth schedule =
            match schedule with
            | ScheduleStep (fac,idx)      ->
                let delta = fac.UpdateMessage idx
                if (depth <= maxDepth) then printf "%sScheduleStep = %.6f\n" (MakeStr depth ' ') (float delta)
                delta
            | ScheduleSeq sseq          ->
                let delta = sseq |> Seq.map (RunInternal (depth+1)) |> Seq.fold max 0.0<_>
                if (depth <= maxDepth) then printf "%sScheduleStep = %.6f\n" (MakeStr depth ' ') (float delta)
                delta
            | ScheduleLoop (s,maxDelta) ->
                let mutable delta = RunInternal (depth+1) s
                let mutable iterCnt = 1
                while delta > maxDelta do
                    delta <- RunInternal (depth+1) s
                    iterCnt <- iterCnt + 1
                done
                if (depth <= maxDepth) then printf "%sScheduleLoop = %.6f (%d iterations)\n" (MakeStr depth ' ') (float delta) iterCnt
                delta

        /// First run the schedule to conversion
        RunInternal 0 schedule
