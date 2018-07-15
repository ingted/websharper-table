namespace CSSPA

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI
open WebSharper.UI.Server
open WebSharper.UI.Html

type SPA =
    | [<EndPoint "cities">] Cities
    | [<EndPoint "cities">] City of string

type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "/spa">] SPA of SPA

[<AutoOpen>]
module Templating =
    open WebSharper.UI.Templating
    open WebSharper.UI.Client

    type MainTemplate = Template<"Main.html", ClientLoad.FromDocument, ServerLoad.WhenChanged>

    let CitiesDropdown (ctx: Context<EndPoint>) =
        let ( => ) (txt: string) act =
            MainTemplate.MenuItem()
                .Title(txt)
                .Link(ctx.Link act)
                .Doc()
        [
            "Home" => EndPoint.Home
            "Cities" => EndPoint.SPA SPA.Cities
        ]

    let Main ctx (title: string) (banner: Doc) (body: Doc list) =
        Content.Page(
            MainTemplate()
                .Title(title)
                .Menu(CitiesDropdown ctx)
                .Banner(banner)
                .Body(body)
                .TableTest(
                    WebSharper.UI.Html.client 
                        <@
                            let data = 
                                ListModel.FromSeq [
                                    "-TEXT_1_IN_COL_1-"
                                    "-TEXT_2_IN_COL_1-"
                                ]
           
                            let tmp =                             
                                MainTemplate.TableTemplate()
                                    .MyTableHole(
                                        data.ViewState.DocSeqCached(
                                            fun (d: string) -> 
                                                MainTemplate.TableTemplateRow()
                                                    .Name(d)                                                    
                                                    .Doc()))

                            tmp.Doc()
                        @>

                )   
                .Doc()
        )

[<JavaScript>]
module Client =
    open WebSharper.UI.Client

    let store =
        [
            "Budapest", "awesome"
            "Paris", "famous"
            "San Francisco", "expensive"
            "London", "cosmopolitan"
            "Singapore", "crowded"
        ]

    let Main () =
        let router = Router.Infer<EndPoint>()
        let location =
            router
            |> Router.Slice (function | SPA spa -> Some spa | _ -> None) EndPoint.SPA
            |> Router.Install SPA.Cities

        location.View.Doc(function
            | SPA.Cities ->
                MainTemplate.CityLinks()
                    .Links(
                        store |> List.map (fun (city, _) ->
                            MainTemplate.CityLink()
                                .Link(router.Link (EndPoint.SPA (SPA.City city)))
                                .Title(city)
                                .Doc()
                        )
                    )
                    .Doc()
            | SPA.City city ->
                let message =
                    match List.tryFind (fst >> (=) city) store with
                    | None ->
                        MainTemplate.NotFound().Doc()
                    | Some (_, adjective) ->
                        MainTemplate.Found().Kind(adjective).Doc()
                MainTemplate.CityPage()
                    .Name(city)
                    .Message(message)
                    .BackLink(router.Link (EndPoint.SPA SPA.Cities))
                    .Doc()
        )

module Site =
    let HomePage ctx =
        Templating.Main ctx "Home" (MainTemplate.HomeBanner().Doc()) [
            p [] [text "There is nothing here, select Cities from the menu."]
        ]

    let CitiesPage ctx =
        Templating.Main ctx "Cities" (MainTemplate.CitiesBanner().Doc()) [
            div [] [client <@ Client.Main () @> ]
        ]

    [<Website>]
    let Main =
        Application.MultiPage (fun ctx endpoint ->
            match endpoint with
            | EndPoint.Home ->
                HomePage ctx
            | EndPoint.SPA SPA.Cities
            | EndPoint.SPA (SPA.City _) ->
                CitiesPage ctx
        ) 