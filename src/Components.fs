namespace App

open Feliz
open Feliz.Router
open Feliz.Plotly
open Feliz.Bulma

module Chart =
    
    type RedOrBlack = Red | Black

    let renderChart bars =
        let red1 = color.rgb(235, 66, 54)
        let red2 = color.rgb(245, 92, 81)
        let red3 = color.rgb(247, 137, 129)
        let black1 = color.rgb(33, 33, 33)
        let black2 = color.rgb(71, 70, 70)
        let black3 = color.rgb(115, 115, 115)
        let reds = [ red1; red2; red3 ]
        let blacks = [ black1; black2; black3 ]
        
        let folder = fun ((nred, nblack), agg) ((redOrBlack: RedOrBlack), label, (values: float<gbp> list)) ->
            let values =
                values
                |> List.map (fun x -> x / 1.0<gbp>)
            let colourRbg = 
                match redOrBlack with
                | Red -> reds.[nred % 3]
                | Black -> blacks.[nblack % 3]

            let trace = traces.bar [
                    bar.x [ "perm"; "inside" ]
                    bar.y values
                    bar.name label
                    bar.marker [ marker.color colourRbg ]
                ]
            let nextRB =
                match redOrBlack with
                | Red -> (nred + 1, nblack)
                | Black -> (nred, nblack + 1)

            (nextRB, agg @ [trace])

        let foldResult: (int * int) * ITracesProperty list = bars |> List.fold folder ((0, 0), [])
        let traces = foldResult |> snd
        
        Plotly.plot [
            plot.traces traces
            plot.layout [
                layout.barmode.stack
            ]
        ]
    
    let renderDetailed perm inside =
        let getTakeHome results = results |> List.choose (fun x -> match x with | Takehome th -> Some th | _ -> None) |> List.exactlyOne
        let getTaxes results = results |> List.choose (fun x -> match x with | Tax t -> Some t | _ -> None)
        let getOtherWealth results = results |> List.choose (fun x -> match x with | OtherWealth t -> Some t | _ -> None)
        
        let permNet = getTakeHome perm
        let insideNet = getTakeHome inside

        let getParts p i rob filter =
            let permParts = filter p
            let insideParts = filter i
            let labels = List.distinct (
                    (List.map (fun x -> x.Label) permParts) @ (List.map (fun x -> x.Label) insideParts)
                )
            
            labels
            |> List.map (fun l -> 
                let getValueOfLabelOrZero parts =
                    parts
                    |> List.filter (fun x -> x.Label = l)
                    |> List.tryExactlyOne
                    |> Option.map (fun x -> x.Amount)
                    |> Option.defaultWith (fun () -> 0.0<gbp>)
                let permValue = getValueOfLabelOrZero permParts
                let insideValue = getValueOfLabelOrZero insideParts
                (rob, l, [ permValue; insideValue ])
            )

        let net = [
            (Black, permNet.Label, [ permNet.Amount; insideNet.Amount ])
        ]
        let taxesParts = getParts perm inside Red getTaxes
        let otherWealthParts = getParts perm inside Black getOtherWealth

        renderChart (net @ otherWealthParts @ taxesParts)

    let renderSimple perm inside =
        let getTakeHome results = results |> List.choose (fun x -> match x with | Takehome th -> Some th | _ -> None) |> List.exactlyOne
        let getOtherWealth results = results |> List.choose (fun x -> match x with | OtherWealth t -> Some t | _ -> None)
        let getTaxes results = results |> List.choose (fun x -> match x with | Tax t -> Some t | _ -> None)
        let getOtherCost results = results |> List.choose (fun x -> match x with | OtherCost t -> Some t | _ -> None)
        
        let permBlack =
            [ getTakeHome perm ] @ getOtherWealth perm
            |> List.sumBy (fun x -> x.Amount)
        
        let permRed =
            getTaxes perm @ getOtherCost perm
            |> List.sumBy (fun x -> x.Amount)
        
        let insideBlack =
            [ getTakeHome inside ] @ getOtherWealth inside
            |> List.sumBy (fun x -> x.Amount)
        
        let insideRed =
            getTaxes inside @ getOtherCost inside
            |> List.sumBy (fun x -> x.Amount)

        renderChart [
            (Black, "Wealth", [permBlack; insideBlack])
            (Red, "Tax", [permRed; insideRed])
        ]


type Components =
    [<ReactComponent>]
    static member Home() =
        let (grossSalary, setGrossSalary) = React.useState(45000.)
        let (bonus, setBonus) = React.useState(10.)
        let (pensionCompany, setPensionCompany) = React.useState(12.)
        let (pensionPersonal, setPensionPersonal) = React.useState(8.)

        let (dayRate, setDayRate) = React.useState(350)
        //let (numberOfWeeks, setNumberOfWeeks) = React.useState(44)
        
        let (showDetailed, setShowDetailed) = React.useState(false)

        let insideParts = calculateInside dayRate true 
        
        let pension = CompanyContribution {
            Personal = (pensionPersonal * 1.0<percent>)
            Company = (pensionCompany * 1.0<percent>)
            }
        let permParts =
            calculatePerm
                ((grossSalary / 12.) * 1.0<gbp>)
                (Some (Bonus.Percentage (bonus * 1.0<percent>)))
                (Some pension)
                false
        
        let grossSalaryChanged = fun (s) -> s |> setGrossSalary
        let bonusChanged = fun (s) -> s |> setBonus
        let pensionPersonalChanged = fun (s) -> s |> setPensionPersonal
        let pensionCompanyChanged = fun (s) -> s |> setPensionCompany
        
        let colWidths = 400
        let colHeight = 600

        let ir35 =
            Html.div [
                prop.style [
                    style.width colWidths
                    //style.height colHeight
                    style.display.inlineBlock
                ]
                prop.children [
                    Bulma.card [
                        Bulma.cardContent [
                            Bulma.content "Inside IR35"
                            Bulma.field.div [
                                Bulma.label "Day rate"
                                Bulma.control.div [
                                    Bulma.input.number [
                                        prop.onChange setDayRate
                                        prop.value dayRate
                                    ]
                                ]
                            ]
                            // Bulma.field.div [
                            //     Bulma.label "Number of weeks"
                            //     Bulma.control.div [
                            //         Bulma.input.number [
                            //             prop.onChange setNumberOfWeeks
                            //             prop.value numberOfWeeks
                            //         ]
                            //     ]
                            // ]
                        ]
                    ]
                ]
            ]
        let perm =
            Html.div [
                prop.style [
                    style.width colWidths
                    style.display.inlineBlock
                ]
                prop.children [
                    Bulma.card [
                        Bulma.cardContent [
                            Bulma.content "Permeanent"
                            Bulma.field.div [
                                Bulma.label "Gross salary"
                                Bulma.control.div [
                                    Bulma.input.number [
                                        prop.onChange grossSalaryChanged
                                        prop.value grossSalary
                                    ]
                                ]
                            ]
                            Bulma.field.div [
                                Bulma.label "Bonus"
                                Bulma.control.div [
                                    Bulma.input.number [
                                        prop.onChange bonusChanged
                                        prop.value bonus
                                    ]
                                ]
                            ]
                            Bulma.field.div [
                                Bulma.label "Pension personal"
                                Bulma.control.div [
                                    Bulma.input.number [
                                        prop.onChange pensionPersonalChanged
                                        prop.value pensionPersonal
                                    ]
                                ]
                            ]
                            Bulma.field.div [
                                Bulma.label "Pension company"
                                Bulma.control.div [
                                    Bulma.input.number [
                                        prop.onChange pensionCompanyChanged
                                        prop.value pensionCompany
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]

        let chartFunc = if showDetailed then Chart.renderDetailed else Chart.renderSimple

        Html.div [
            Bulma.navbar [
                Bulma.color.isPrimary
                prop.children [
                    Bulma.navbarBrand.div [
                        Bulma.navbarItem.a [
                            Html.h3 [ prop.text "Perm vs inside"; prop.height 28; prop.width 112; ]
                        ]
                    ]
                ]
            ]

            ir35
            perm

            Html.div [
                prop.children [
                    Bulma.card [
                        Bulma.cardContent [
                            Bulma.field.div [
                                Bulma.label "Show detailed"
                                Bulma.control.div [
                                    Bulma.input.checkbox [
                                        prop.value showDetailed
                                        prop.onChange setShowDetailed
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            
            Html.div [
                chartFunc permParts insideParts
            ]
        ]

    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
        React.router [
            router.onUrlChanged updateUrl
            router.children [
                match currentUrl with
                | [ ] -> Components.Home()
                //| [ "hello" ] -> Components.HelloWorld()
                //| [ "counter" ] -> Components.Counter()
                | otherwise -> Html.h1 "Not found"
            ]
        ]