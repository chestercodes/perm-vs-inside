[<AutoOpen>]
module Calculate

open System
open Fable.Core
open Fable.Core.JsInterop

[<Measure>] type gbp
[<Measure>] type percent

type Bonus = 
    | Percentage of float<percent>
    | AnnualLumpSum of float<gbp>

type CompanyContribution = {
    Personal: float<percent>
    Company: float<percent>
    }

let percentageOfAmount (amount: float<gbp>) (per: float<percent>) =
    (per / 100.<percent>) * amount

type Pension = 
    | CompanyContribution of CompanyContribution
    // | Lump of float

type ResultPart = { Amount: float<gbp>; Label: string } 

type CalculationResultPart =
    | Takehome of ResultPart
    | OtherWealth of ResultPart
    //| TimeOffEquivalent of ResultPart
    | Tax of ResultPart 
    | OtherCost of ResultPart 

let calculatePension
    (gross: float<gbp>)
    (pensionOpt: Pension option) =
    match pensionOpt with
    | Some pension ->
        match pension with
        | CompanyContribution contribution ->
            let personalPension = percentageOfAmount gross contribution.Personal
            let companyPension = percentageOfAmount gross contribution.Company
            let makeNet = 75.<percent>
            [
                OtherWealth {
                    Amount = percentageOfAmount personalPension makeNet
                    Label = "Personal pension (gross)"
                }
                OtherWealth {
                    Amount = percentageOfAmount companyPension makeNet
                    Label = "Company pension (gross)"
                }
            ]          
    | None -> []
    
let calculateTaxAndTakehome
    (gross: float<gbp>)
    (pensionOpt: Pension option) =
    
    let returnNetAmount netAmount =
        Takehome {
            Amount = netAmount
            Label = "Net amount"
        }

    let grossForTax = 
        match pensionOpt with
        | Some pension ->
            match pension with
            | CompanyContribution contribution ->
                gross - (percentageOfAmount gross contribution.Personal)
        | None -> gross
    printfn "After pension and bonus %f" grossForTax
    
    let taxFreeAllowance =
        let maxLower = 12579.<gbp>
        match grossForTax with
        | g when g < 100000.<gbp> -> maxLower  
        | g when g > 125000.<gbp> -> 0.<gbp>
        | g -> 
            printfn "Need to calc tax allowance"
            let diff = g - 100000.<gbp>
            printfn "Difference to lower %f" diff
            let deduction = Convert.ToInt32(System.Math.Round((diff / 1.<gbp>)))
            printfn "Deduction %i" deduction
            let amount = maxLower - (float (deduction / 2) * 1.<gbp>)
            printfn "Amount %f" amount
            amount
    printfn "Tax free allowance %f" taxFreeAllowance
    
    let getIncomeBracketsAndAmounts taxFree = [
            (0.<gbp>,      Some taxFree,      0.0<percent>)
            (taxFree,      Some 50270.<gbp>,  20.0<percent>)
            (50270.<gbp>,  Some 150000.<gbp>, 40.0<percent>)
            (150000.<gbp>, None,              45.0<percent>)
        ]

    let nIBracketsAndAmounts = [
            (0.<gbp>,     Some 9568.<gbp>,  0.0<percent>)
            (9568.<gbp>,  Some 50270.<gbp>, 12.0<percent>)
            (50270.<gbp>, None,             2.0<percent>)
        ]

    let incomeTaxBracketsAndAmounts = getIncomeBracketsAndAmounts taxFreeAllowance
    
    let splitTaxes = fun (lower, upperOpt, percentage) -> 
        let bracketGross = 
            match upperOpt with
            | None -> 
                if grossForTax > lower then grossForTax - lower else 0.<gbp>
            | Some upper ->
                match grossForTax with
                | g when g < lower -> 0.<gbp>
                | g when g > upper -> upper - lower
                | g -> g - lower
        (bracketGross, percentage)

    let incomeTaxes =
        incomeTaxBracketsAndAmounts 
        |> List.map (fun tax -> 
            let (bracketGross, percentage) = splitTaxes tax
            let amount = percentageOfAmount bracketGross percentage
            {
                Amount = amount
                Label = (sprintf "Income Tax @ %f" percentage)
            }
        )

    let nITaxes =
        nIBracketsAndAmounts 
        |> List.map (fun tax -> 
            let (bracketGross, percentage) = splitTaxes tax
            let amount = percentageOfAmount bracketGross percentage
            {
                Amount = amount
                Label = (sprintf "NI contribution @ %f" percentage)
            }
        )
    
    let allTaxes = incomeTaxes @ nITaxes //|> List.filter (fun g -> g.Amount > 0.)
    let totalTax = allTaxes |> List.sumBy (fun g -> g.Amount)

    [
        [ returnNetAmount (grossForTax - totalTax) ]
        allTaxes |> List.map Tax
    ]
    |> List.collect id

let calculatePerm
    gross
    (bonusOpt: Bonus option)
    (pensionOpt: Pension option) =
    
    let totalGross =
        match bonusOpt with
        | None -> gross
        | Some (Bonus.Percentage bonus) -> gross + (percentageOfAmount gross bonus)
        | Some (AnnualLumpSum bonus) -> gross + bonus
    printfn "Total gross %f" totalGross
    
    [
        calculateTaxAndTakehome totalGross pensionOpt
        calculatePension totalGross pensionOpt
    ]
    |> List.collect id

let calculateInside
    dayRate
    weeksWorked =
    let gross = dayRate * 5 * weeksWorked |> float
    calculatePerm (gross * 1.<gbp>) None None