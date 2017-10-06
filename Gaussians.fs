//--------------------------------------------------------------
// gaussians.fs           Gaussian variables and factors
//
// 2007/2008 written by Ralf Herbrich
//
// Copyright (c) 2002-2011 Microsoft Research Ltd.
//
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
//
//--------------------------------------------------------------

namespace MSRC.Inference.Distributions

open System
open System.IO
open MSRC.Inference

/// A Gaussian distribution based on float numbers (struct type for memory efficency) 
/// in exponential parameterisation. 
type Gaussian<[<Measure>] 'a> = 
    struct
        /// Precision times the mean of the Gaussian
        val PrecisionMean   : float<1/'a>
        /// Precision of the Gaussian
        val Precision       : float<1/'a^2>
    end
    with
        /// Constructor using the precision times mean and the precision
        new (pm,p) = { PrecisionMean = pm; Precision = p } 
        /// Mean of the Gaussian
        member this.Mu = this.PrecisionMean / this.Precision
        /// Mean of the Gaussian
        member this.Mean = this.Mu
        /// Variance of the Gaussian
        member this.Variance : float<'a^2> = 1.0 / this.Precision
        /// Standard deviation of the Gaussian
        member this.StandardDeviation = sqrt (this.Variance)
        /// Standard deviation of the Gaussian
        member this.Sigma = this.StandardDeviation

        /// Creates a Gaussian in (mean,standard-deviation) coordinates
        static member Create (mu:float<'a>,sigma:float<'a>) = 
            let sigmaSquared = sigma * sigma
            new Gaussian<'a> (mu / sigmaSquared, 1.0 / sigmaSquared)
        /// Multiplies two Gaussians  
        static member ( * ) (a:Gaussian<'a>,b:Gaussian<'a>) = 
            new Gaussian<'a> (a.PrecisionMean + b.PrecisionMean, a.Precision + b.Precision)
        /// Divides two Gaussians
        static member (/) (a:Gaussian<'a>,b:Gaussian<'a>) =
            new Gaussian<'a> (a.PrecisionMean - b.PrecisionMean, a.Precision - b.Precision)
        /// Computes the absolute difference between two Gaussians
        static member AbsoluteDifference (a:Gaussian<'a>,b:Gaussian<'a>) = 
            max (abs (a.PrecisionMean - b.PrecisionMean)) (sqrt (abs (a.Precision - b.Precision)))
        /// Computes the absolute difference between two Gaussians
        static member (-) (a:Gaussian<'a>,b:Gaussian<'a>) = Gaussian.AbsoluteDifference (a,b)
        /// Computes the log-normalisation factor when two normalised Gaussians gets multiplied
        static member LogProductNormalisation (a:Gaussian<'a>,b:Gaussian<'a>) =
            if a.Precision = 0.0<_> then 
                0.0
            elif b.Precision = 0.0<_> then
                0.0
            else
                let varSum = a.Variance + b.Variance
                let muDiff = a.Mean - b.Mean
                -0.91893853320467267 - log(float varSum)/2.0 - muDiff*muDiff/(2.0 * varSum)
        /// Computes the log-normalisation factor when two normalised Gaussians gets divided
        static member LogRatioNormalisation (a:Gaussian<'a>,b:Gaussian<'a>) =
            if a.Precision = 0.0<_> then 
                0.0
            elif b.Precision = 0.0<_> then
                0.0
            else
                let v2 = b.Variance
                let varDiff = v2 - a.Variance
                let muDiff = a.Mean - b.Mean
                if varDiff = 0.0<_> then
                    0.0
                else
                    log(float v2) + 0.91893853320467267 - log(float varDiff)/2.0 + muDiff*muDiff/(2.0 * varDiff)

module Gaussians =
    /// Computes the complementary error function. This function is defined     
    /// by 2/sqrt(pi) * integral from x to infinity of exp (-t^2) dt
    let erfc x =
        if (System.Double.IsNegativeInfinity (x)) then
            2.0
        else if (System.Double.IsPositiveInfinity (x)) then
            0.0
        else
            let z = abs (x) 
            let t = 1.0 / (1.0 + 0.5 * z) 
            let res = t * exp (-z * z - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277))))))))) 
            if (x >= 0.0) then res else 2.0 - res

    /// Computes the inverse of the complementary error function
    let erfcinv y = 
        if (y < 0.0 || y > 2.0) then
            failwith "Inverse complementary function not defined outside [0,2]."
        elif (y = 0.0) then
            System.Double.PositiveInfinity
        elif (y = 2.0) then
            System.Double.NegativeInfinity
        else 
            let x = 
                if (y >= 0.0485 && y <= 1.9515) then
                    let q = y - 1.0 
                    let r = q * q 
                    (((((0.01370600482778535*r - 0.3051415712357203)*r + 1.524304069216834)*r - 3.057303267970988)*r + 2.710410832036097)*r - 0.8862269264526915) * q /
                    (((((-0.05319931523264068*r + 0.6311946752267222)*r - 2.432796560310728)*r + 4.175081992982483)*r - 3.320170388221430)*r + 1.0)
                else if (y < 0.0485) then
                    let q = sqrt (-2.0 * log (y / 2.0)) 
                    (((((0.005504751339936943*q + 0.2279687217114118)*q + 1.697592457770869)*q + 1.802933168781950)*q + -3.093354679843504)*q - 2.077595676404383) / 
                    ((((0.007784695709041462*q + 0.3224671290700398)*q + 2.445134137142996)*q + 3.754408661907416)*q + 1.0)
                else if (y > 1.9515) then
                    let q = sqrt (-2.0 * log (1.0 - y / 2.0)) 
                    (-(((((0.005504751339936943*q + 0.2279687217114118)*q + 1.697592457770869)*q + 1.802933168781950)*q + -3.093354679843504)*q - 2.077595676404383) / 
                     ((((0.007784695709041462*q + 0.3224671290700398)*q + 2.445134137142996)*q + 3.754408661907416)*q + 1.0))
                else 0.0
            let u = (erfc (x) - y) / (-2.0 / sqrt (System.Math.PI) * exp (-x * x)) 
            x - u / (1.0 + x * u)

    /// Computes the cummulative Gaussian distribution at a specified point of interest
    let normcdf t = 
        let sqrt2 = 1.4142135623730951 
        (erfc (-t / sqrt2)) / 2.0

    /// Computes the Gaussian density at a specified point of interest
    let normpdf (t:float) = 
        let invsqrt2pi = 0.398942280401433
        invsqrt2pi * exp (- (t * t / 2.0))
        
    /// Computes the inverse of the cummulative Gaussian distribution (qunatile function) at a specified point of interest
    let norminv p = 
        let sqrt2 = 1.4142135623730951 
        (-sqrt2 * erfcinv (2.0 * p))

    /// Computes the cummulative Gaussian distribution at a specified point of interest
    let Phi = normcdf
        
    /// Computes the inverse of the cummulative Gaussian distribution (qunatile function) at a specified point of interest
    let PhiInverse = norminv


    /// Computes the additive correction of a single-sided truncated Gaussian with unit variance
    let v t epsilon = 
        match normcdf (t-epsilon) with
        | denom when denom < 2.222758749e-162   -> -t+epsilon
        | denom                                 -> (normpdf (t - epsilon)) / denom                             

    /// Computes the multiplicative correction of a single-sided truncated Gaussian with unit variance
    let w t epsilon =
        match normcdf (t-epsilon) with
        | denom when denom < 2.222758749e-162   -> if (t < 0.0) then 1.0 else 0.0
        | denom                                 -> let vt = v t epsilon in vt * (vt + t - epsilon)

    /// Computes the additive correction of a double-sided truncated Gaussian with unit variance
    let v0 t epsilon = 
        let v = abs t
        match normcdf (epsilon - v) - normcdf (-epsilon - v) with
        | denom when denom < 2.222758749e-162   -> if t < 0.0 then -t-epsilon else -t+epsilon
        | denom                                 -> let num = normpdf (-epsilon-v) - normpdf (epsilon-v) in if t < 0.0 then -num/denom else num/denom

    /// Computes the multiplicative correction of a double-sided truncated Gaussian with unit variance
    let w0 t epsilon =
        let v = abs t
        match normcdf (epsilon - v) - normcdf (-epsilon - v) with
        | denom when denom < 2.222758749e-162   -> 1.0
        | denom                                 -> let vt = v0 v epsilon in vt*vt + ((epsilon-v) * normpdf (epsilon-v) - (-epsilon-v) * normpdf (-epsilon-v))/denom

namespace MSRC.Inference

open MSRC.Inference.Collections
open MSRC.Inference.Factors
open MSRC.Inference.Distributions
open MSRC.Inference.Distributions.Gaussians
    
type GaussianFactors<[<Measure>] 'a> () = 
    /// Defines a bag of distributions that holds all the Gaussian message 
    let MessageBag = new DistributionBag<_> (new Gaussian<'a> (0.0<_>, 0.0<_>))
    
    /// Allocates a new Gausian with parameter Mean=mu and Variance=sigma2
    let GaussianMS (mu:float<'a>,sigma2:float<'a^2>) = new Gaussian<'a> (mu/sigma2, 1.0/sigma2)

    /// Sends the message witha given index to a variable and returns the log-normalisation constant
    let SendMessageHelper msgIdx (varIdx,varBag:DistributionBag<Gaussian<'a>>) = 
        let mar = varBag.[varIdx]
        let msg = MessageBag.[msgIdx] 
        let logZ = Gaussian.LogProductNormalisation (mar, msg)
        varBag.[varIdx] <- mar * msg
        logZ          

    /// Removes all messages stored so far
    member this.ClearMessages () = MessageBag.Reset ()    

    /// Connects a Gaussian priot factor to variable and returns the update function
    member this.GaussianPriorFactor (mu,sigma2) (varIdx,varBag:DistributionBag<Gaussian<'a>>) = 
        let msgIdx = MessageBag.NewDistribution ()
        let newMsg = GaussianMS (mu, sigma2)
        { 
            UpdateMessage = fun idx ->
                match idx with
                | 0 -> 
                    let oldMarginal = varBag.[varIdx]
                    let oldMsg = MessageBag.[msgIdx]
                    let newMarginal = new Gaussian<'a> (oldMarginal.PrecisionMean + newMsg.PrecisionMean - oldMsg.PrecisionMean, oldMarginal.Precision + newMsg.Precision - oldMsg.Precision)
                    varBag.[varIdx] <- newMarginal
                    MessageBag.[msgIdx] <- newMsg
                    oldMarginal - newMarginal
                | _ -> failwith "Variable index out of range. Valid values are 0 only."
            LogNormalisation = fun () -> 0.0
            NumberOfMessages = 1
            ResetMarginals = fun () -> varBag.[varIdx] <- varBag.Prior
            SendMessage = fun idx ->
                match idx with
                | 0 -> SendMessageHelper msgIdx (varIdx,varBag) 
                | _ -> failwith "Variable index out of range. Valid values are 0 only."
        }

    /// Connects a Gaussian likelihood factor to 2 variables and returns the update function
    /// The distributions are related by N(var0;var1,betaSquared).
    member this.GaussianLikelihoodFactor betaSquared (var1Idx,var1Bag:DistributionBag<Gaussian<'a>>) (var2Idx,var2Bag:DistributionBag<Gaussian<'a>>) = 
        let msg1Idx = MessageBag.NewDistribution ()
        let msg2Idx = MessageBag.NewDistribution ()
        let prec = 1.0<_> / betaSquared 
        let UpdateHelper m1 m2 (v1,b1:DistributionBag<Gaussian<_>>) (v2,b2:DistributionBag<Gaussian<_>>) = 
            let msg1 = MessageBag.[m1]
            let msg2 = MessageBag.[m2]
            let mar1 = b1.[v1]
            let mar2 = b2.[v2]
            let a = prec / (prec + mar2.Precision - msg2.Precision)
            let newMsg = Gaussian (a * (mar2.PrecisionMean - msg2.PrecisionMean), a * (mar2.Precision - msg2.Precision))
            let oldMarginalWithoutMsg = mar1 / msg1
            let newMarginal = oldMarginalWithoutMsg * newMsg 
            /// Update the message and marginal
            MessageBag.[m1] <- newMsg
            b1.[v1] <- newMarginal
            /// Return the difference in the new marginal
            newMarginal - mar1
        {
            UpdateMessage = fun idx ->
                match idx with
                | 0 ->  UpdateHelper msg1Idx msg2Idx (var1Idx,var1Bag) (var2Idx,var2Bag)
                | 1 ->  UpdateHelper msg2Idx msg1Idx (var2Idx,var2Bag) (var1Idx,var1Bag)
                | _ ->  failwith "Variable index out of range. Valid values are 0 and 1."
            LogNormalisation = fun () -> Gaussian.LogRatioNormalisation (var1Bag.[var1Idx], MessageBag.[msg1Idx])
            NumberOfMessages = 2
            ResetMarginals = fun () ->
                var1Bag.[var1Idx] <- var1Bag.Prior
                var2Bag.[var2Idx] <- var2Bag.Prior
            SendMessage = fun idx -> 
                match idx with
                | 0 -> SendMessageHelper msg1Idx (var1Idx,var1Bag) 
                | 1 -> SendMessageHelper msg2Idx (var2Idx,var2Bag) 
                | _ ->  failwith "Variable index out of range. Valid values are 0 and 1."
        }            
        
    /// Connects a Gaussian weighted sum factor to 3 variables and returns the update function
    /// The distributions are related by var0 = a1*var1 + a2*var2
    member this.GaussianWeightedSumFactor a1 a2 (var0Idx,var0Bag:DistributionBag<Gaussian<'a>>) (var1Idx,var1Bag:DistributionBag<Gaussian<'a>>) (var2Idx,var2Bag:DistributionBag<Gaussian<'a>>) = 
        let msg0Idx = MessageBag.NewDistribution ()
        let msg1Idx = MessageBag.NewDistribution ()
        let msg2Idx = MessageBag.NewDistribution ()
        let weights0 = [| a1; a2 |]
        let weights1 = [| -a2/a1; 1.0/a1 |]
        let weights2 = [| -a1/a2; 1.0/a2 |]
        let weights0Squared = [| weights0.[0]*weights0.[0]; weights0.[1]*weights0.[1] |]
        let weights1Squared = [| weights1.[0]*weights1.[0]; weights1.[1]*weights1.[1] |]
        let weights2Squared = [| weights2.[0]*weights2.[0]; weights2.[1]*weights2.[1] |]
        let UpdateHelper (w:float array) (wS:float array) m1 m2 m3 (v1,b1:DistributionBag<Gaussian<_>>) (v2,b2:DistributionBag<Gaussian<_>>) (v3,b3:DistributionBag<Gaussian<_>>) = 
            let d0 = b2.[v2] / MessageBag.[m2]
            let d1 = b3.[v3] / MessageBag.[m3]
            let msg1 = MessageBag.[m1]
            let mar1 = b1.[v1]
            let denom = wS.[0] * d1.Precision + wS.[1] * d0.Precision
            let newPrecision = d0.Precision * d1.Precision / denom
            let newPrecisionMean = (w.[0] * d1.Precision * d0.PrecisionMean + w.[1] * d0.Precision * d1.PrecisionMean) / denom
            let newMsg = Gaussian (newPrecisionMean, newPrecision)
            let oldMarginalWithoutMsg = mar1 / msg1 
            let newMarginal = oldMarginalWithoutMsg * newMsg 
            /// Update the message and marginal
            MessageBag.[m1] <- newMsg
            b1.[v1] <- newMarginal
            /// Return the difference in the new marginal
            newMarginal - mar1
        {
            UpdateMessage = fun idx ->
                match idx with
                | 0 ->  UpdateHelper weights0 weights0Squared msg0Idx msg1Idx msg2Idx (var0Idx,var0Bag) (var1Idx,var1Bag) (var2Idx,var2Bag)
                | 1 ->  UpdateHelper weights1 weights1Squared msg1Idx msg2Idx msg0Idx (var1Idx,var1Bag) (var2Idx,var2Bag) (var0Idx,var0Bag)
                | 2 ->  UpdateHelper weights2 weights2Squared msg2Idx msg1Idx msg0Idx (var2Idx,var2Bag) (var1Idx,var1Bag) (var0Idx,var0Bag)
                | _ ->  failwith "Variable index out of range. Valid values are 0, 1 and 2."
            LogNormalisation = fun () -> 
                Gaussian.LogRatioNormalisation (var1Bag.[var1Idx], MessageBag.[msg1Idx]) + Gaussian.LogRatioNormalisation (var2Bag.[var2Idx], MessageBag.[msg2Idx])
            NumberOfMessages = 3
            ResetMarginals = fun () ->
                var0Bag.[var0Idx] <- var0Bag.Prior
                var1Bag.[var1Idx] <- var1Bag.Prior
                var2Bag.[var2Idx] <- var2Bag.Prior
            SendMessage = fun idx ->
                match idx with
                | 0 -> SendMessageHelper msg0Idx (var0Idx,var0Bag) 
                | 1 -> SendMessageHelper msg1Idx (var1Idx,var1Bag) 
                | 2 -> SendMessageHelper msg2Idx (var2Idx,var2Bag) 
                | _ ->  failwith "Variable index out of range. Valid values are 0, 1 and 2."
        }            
            
    /// Connects a Gaussian greater-than-epsilon factor to variable and returns the update function
    member this.GaussianGreaterThanFactor epsilon (varIdx,varBag:DistributionBag<Gaussian<'a>>) =
        let msgIdx = MessageBag.NewDistribution ()
        {
            UpdateMessage = fun idx ->
                match idx with
                | 0 ->  
                    let oldMarginal = varBag.[varIdx]
                    let oldMsg = MessageBag.[msgIdx]
                    let msgFromVar = oldMarginal / oldMsg
                    let c = msgFromVar.Precision
                    let d = msgFromVar.PrecisionMean
                    let sqrtC = sqrt c
                    let dOnSqrtC = d / sqrtC
                    let epsTimesSqrtC = epsilon * sqrtC
                    let d = msgFromVar.PrecisionMean
                    let denom = 1.0 - w dOnSqrtC epsTimesSqrtC
                    let newPrecision = c / denom
                    let newPrecisionMean = (d + sqrtC * v dOnSqrtC epsTimesSqrtC) / denom
                    let newMarginal = Gaussian (newPrecisionMean, newPrecision)
                    let newMsg = oldMsg * newMarginal / oldMarginal
                    /// Update the message and marginal
                    MessageBag.[msgIdx] <- newMsg
                    varBag.[varIdx] <- newMarginal
                    /// Return the difference in the new marginal
                    newMarginal - oldMarginal
                | _ ->  failwith "Variable index out of range. Valid values are 0 only."
            LogNormalisation = fun () -> 
                let marginal = varBag.[varIdx]
                let msg = MessageBag.[msgIdx]
                let msgFromVar = marginal / msg
                -Gaussian.LogProductNormalisation (msgFromVar, msg) + log(normcdf ((msgFromVar.Mean - epsilon)/msgFromVar.StandardDeviation)) 
            NumberOfMessages = 1
            ResetMarginals = fun () ->
                varBag.[varIdx] <- varBag.Prior
            SendMessage = fun idx ->
                match idx with
                | 0 -> SendMessageHelper msgIdx (varIdx,varBag) 
                | _ ->  failwith "Variable index out of range. Valid values are 0 only."
        }            

    /// Connects a Gaussian within-epsilon factor to variable and returns the update function
    member this.GaussianWithinFactor epsilon (varIdx,varBag:DistributionBag<Gaussian<'a>>) =
        let msgIdx = MessageBag.NewDistribution ()
        {
            UpdateMessage = fun idx ->
                match idx with
                | 0 ->  
                    let oldMarginal = varBag.[varIdx]
                    let oldMsg = MessageBag.[msgIdx]
                    let msgFromVar = oldMarginal / oldMsg
                    let c = msgFromVar.Precision
                    let d = msgFromVar.PrecisionMean
                    let sqrtC = sqrt c
                    let dOnSqrtC = d / sqrtC
                    let epsTimesSqrtC = epsilon * sqrtC
                    let d = msgFromVar.PrecisionMean
                    let denom = 1.0 - w0 dOnSqrtC epsTimesSqrtC
                    let newPrecision = c / denom
                    let newPrecisionMean = (d + sqrtC * v0 dOnSqrtC epsTimesSqrtC) / denom
                    let newMarginal = Gaussian (newPrecisionMean, newPrecision)
                    let newMsg = oldMsg * newMarginal / oldMarginal
                    /// Update the message and marginal
                    MessageBag.[msgIdx] <- newMsg
                    varBag.[varIdx] <- newMarginal
                    /// Return the difference in the new marginal
                    newMarginal - oldMarginal
                | _ ->  failwith "Variable index out of range. Valid values are 0 only."
            LogNormalisation = fun () -> 
                let marginal = varBag.[varIdx]
                let msg = MessageBag.[msgIdx]
                let msgFromVar = marginal / msg
                let mean = msgFromVar.Mean
                let std = msgFromVar.StandardDeviation
                let Z = normcdf ((epsilon - mean)/std) - normcdf ((-epsilon-mean)/std)
                -Gaussian.LogProductNormalisation (msgFromVar, msg) + log(Z)
            NumberOfMessages = 1
            ResetMarginals = fun () ->
                varBag.[varIdx] <- varBag.Prior
            SendMessage = fun idx ->
                match idx with
                | 0 -> SendMessageHelper msgIdx (varIdx,varBag) 
                | _ ->  failwith "Variable index out of range. Valid values are 0 only."
        }