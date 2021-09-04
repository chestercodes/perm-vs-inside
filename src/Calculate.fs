[<AutoOpen>]
module Calculate

open System
open Fable.Core
open Fable.Core.JsInterop

type Bonus = 
    | Percentage of float
    | AnnualLumpSum of float

type PensionContribution = 
    | Percentage of float
    | Lump of float

type CompanyContribution = { Personal: float; Company: float option }

type Pension = 
    | CompanyContribution of CompanyContribution
    // | Lump of float

type ResultPart = { Amount: float; Label: string } 

type CalculationResultPart =
    | Takehome of ResultPart
    | OtherWealth of ResultPart
    //| TimeOffEquivalent of ResultPart
    | Tax of ResultPart 
    | OtherCost of ResultPart 

let calculateTaxAndTakehome
    gross
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
                gross - contribution.Personal * 100.0 * gross
        | None -> gross
    
    let taxFreeAllowance =
        let maxLower = 12579.
        match grossForTax with
        | x when x < 100000. -> maxLower  
        | x when x > 125000. -> 0.
        | x -> 
            let diff = x - 100000.
            let deduction = Convert.ToInt32(System.Math.Round(diff))
            let amount = maxLower - float (deduction / 2)
            amount

    let getIncomeBracketsAndAmounts taxFree = [
            (0., Some taxFree, 0.0)
            (taxFree, Some 50270., 20.0)
            (50270., Some 150000., 40.0)
            (150000., None, 45.0)
        ]

    let nIBracketsAndAmounts = [
            (0., Some 9568., 0.0)
            (9568., Some 50270., 12.0)
            (50270., None, 2.0)
        ]

    let incomeTaxBracketsAndAmounts = getIncomeBracketsAndAmounts taxFreeAllowance
    
    let splitTaxes = fun (lower, upperOpt, percentage) -> 
        let bracketGross = 
            match upperOpt with
            | None -> 
                if grossForTax > lower then grossForTax - lower else 0.
            | Some upper ->
                match grossForTax with
                | x when x < lower -> 0.
                | x when x > upper -> upper - lower
                | g -> g - lower
        (bracketGross, percentage)

    let incomeTaxes =
        incomeTaxBracketsAndAmounts 
        |> List.map (fun tax -> 
            let (bracketGross, percentage) = splitTaxes tax
            let amount = bracketGross * percentage
            {
                Amount = amount
                Label = (sprintf "Income Tax @ %f" percentage)
            }
        )

    let nITaxes =
        nIBracketsAndAmounts 
        |> List.map (fun tax -> 
            let (bracketGross, percentage) = splitTaxes tax
            let amount = bracketGross * percentage
            {
                Amount = amount
                Label = (sprintf "NI contribution @ %f" percentage)
            }
        )
    
    let allTaxes = incomeTaxes @ nITaxes //|> List.filter (fun x -> x.Amount > 0.)
    let totalTax = allTaxes |> List.sumBy (fun x -> x.Amount)

    [
        [
            returnNetAmount (grossForTax - totalTax)
        ]
        allTaxes |> List.map Tax
    ]
    |> List.collect id

let calculatePerm
    gross
    daysOff
    (bonus: Bonus option)
    (pension: Pension option) =
    
    [
        Takehome {
            Amount = 1.23
            Label = "Net amount"
        }
    ]

let calculateInside
    dayRate
    weeksWorked =
    
    [
        Takehome {
            Amount = 1.23
            Label = "Net amount"
        }
    ]