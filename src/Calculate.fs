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
    (pensionOpt: Pension option)
    removeEmployerNI =
    
    let returnNetAmount netAmount =
        Takehome {
            Amount = netAmount
            Label = "Net amount"
        }
    
    let splitTaxes grossVal = fun (lower, upperOpt, percentage, label) -> 
        let bracketGross = 
            match upperOpt with
            | None -> 
                if grossVal > lower then grossVal - lower else 0.<gbp>
            | Some upper ->
                match grossVal with
                | g when g < lower -> 0.<gbp>
                | g when g > upper -> upper - lower
                | g -> g - lower
        (bracketGross, percentage)

    let employerNIBracketsAndAmounts = [
            (0.<gbp>,    Some 737.<gbp>,  0.0<percent>,  "Employer NI @ 0%")
            (737.<gbp>,  Some 4189.<gbp>, 13.8<percent>, "Employer NI @ 13.8%")
            (4189.<gbp>, None,            13.8<percent>, "Employer NI (top) @ 13.8%")
        ]

    let applyTaxes gross taxes =
        taxes 
        |> List.map (fun tax -> 
            let (_, _, _, label) = tax
            let (bracketGross, percentage) = splitTaxes gross tax
            let amount = percentageOfAmount bracketGross percentage
            {
                Amount = amount
                Label = label
            }
        )

    let employerNIParts =
        if removeEmployerNI then
            applyTaxes gross employerNIBracketsAndAmounts
        else []
    let employersNITotal = employerNIParts |> List.sumBy (fun a -> a.Amount)

    let grossForTax = 
        match pensionOpt with
        | Some pension ->
            match pension with
            | CompanyContribution contribution ->
                gross - employersNITotal - (percentageOfAmount gross contribution.Personal)
        | None -> gross - employersNITotal
    printfn "After employers NI, pension and bonus %f" grossForTax
    
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
            let amount = (maxLower - (float (deduction / 2) * 1.<gbp>))
            printfn "Amount %f per year" amount
            amount / 12.
    printfn "Tax free allowance %f" taxFreeAllowance
    
    let yearToMonth ((a: float<gbp>), (b: float<gbp> option), (c: float<percent>), d) =
        let upper = Option.map(fun u -> u / 12.) b
        (a / 12., upper, c, d)

    let getIncomeBracketsAndAmounts taxFree =
        let yearValues = [
            (0.<gbp>,      Some taxFree,      0.0<percent>, "Untaxed income")
            (taxFree,      Some 50270.<gbp>,  20.0<percent>, "Income tax @ 20%")
            (50270.<gbp>,  Some 150000.<gbp>, 40.0<percent>, "Income tax @ 40%")
            (150000.<gbp>, None,              45.0<percent>, "Income tax @ 45%")
        ]
        yearValues |> List.map yearToMonth

    let employeeNIBracketsAndAmounts =
        [
            (0.<gbp>,    Some 737.<gbp>,  0.0<percent>, "Employee NI @ 0%")
            (737.<gbp>,  Some 4189.<gbp>, 12.0<percent>, "Employee NI @ 12%")
            (4189.<gbp>, None,             2.0<percent>, "Employee NI @ 2%")
        ]
        |> List.map yearToMonth

    let incomeTaxBracketsAndAmounts = getIncomeBracketsAndAmounts taxFreeAllowance

    let incomeTaxes =
        incomeTaxBracketsAndAmounts 
        |> List.map (fun tax -> 
            let (bracketGross, percentage) = splitTaxes grossForTax tax
            let amount = percentageOfAmount bracketGross percentage
            {
                Amount = amount
                Label = (sprintf "Income Tax @ %f" percentage)
            }
        )

    let employeeNITaxes =
        employeeNIBracketsAndAmounts 
        |> List.map (fun tax -> 
            let (bracketGross, percentage) = splitTaxes grossForTax tax
            let amount = percentageOfAmount bracketGross percentage
            {
                Amount = amount
                Label = (sprintf "Employee NI @ %f" percentage)
            }
        )
    
    let allTaxes = incomeTaxes @ employeeNITaxes @ employerNIParts |> List.filter (fun g -> g.Amount > 0.<gbp>)
    let totalTax = allTaxes |> List.sumBy (fun g -> g.Amount)

    [
        [ returnNetAmount (grossForTax - totalTax) ]
        allTaxes |> List.map Tax
    ]
    |> List.collect id

let calculatePerm
    gross
    (bonusOpt: Bonus option)
    (pensionOpt: Pension option)
    removeEmployerNI =
    
    let totalGross =
        match bonusOpt with
        | None -> gross
        | Some (Bonus.Percentage bonus) -> gross + (percentageOfAmount gross bonus)
        | Some (AnnualLumpSum bonus) -> gross + bonus
    printfn "Total gross %f" totalGross
    
    [
        calculateTaxAndTakehome totalGross pensionOpt removeEmployerNI
        calculatePension totalGross pensionOpt
    ]
    |> List.collect id

let calculateInside dayRate =
    let workDaysPerMonth = 5 * 4
    let gross = dayRate * workDaysPerMonth |> float
    calculatePerm (gross * 1.<gbp>) None None