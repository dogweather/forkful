---
title:                "HTML Parsen"
aliases:
- nl/elm/parsing-html.md
date:                  2024-01-28T22:03:54.095470-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML Parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
HTML parsen betekent het converteren van HTML-tekst naar een datastructuur waarmee je programma kan werken. Programmeurs doen dit om de inhoud van webpagina's programmatisch te manipuleren, extraheren en ermee te interacteren.

## Hoe:
Elm parseert niet uit zichzelf rauwe HTML; in plaats daarvan richt het zich op het renderen van views met zijn eigen `Html` module. Om HTML te parsen, gebruik je doorgaans een server-side service of een externe JavaScript-bibliotheek, waarna je de gegevens naar Elm doorgeeft. Hier is een basis Elm-opzet voor het omgaan met geparseerde HTML-gegevens:

```Elm
module Hoofd blootstellend (hoofd)

importeer Html blootstellend (Html, tekst)
importeer Json.Decode als Decode

type alias HtmlGegevens =
    { tag : String
    , attributen : Lijst (String, String)
    , inhoud : String
    }

-- Uitgaande dat je een JSON-object hebt dat je HTML-gegevens vertegenwoordigt
htmlGegevensDecoder : Decode.Decoder HtmlGegevens
htmlGegevensDecoder =
    Decode.map3 HtmlGegevens
        (Decode.veld "tag" Decode.tekst)
        (Decode.veld "attributen" (Decode.lijst (Decode.tuple Decode.tekst Decode.tekst)))
        (Decode.veld "inhoud" Decode.tekst)

-- Vervang met de daadwerkelijke JSON van een server of externe JS-bibliotheek
voorbeeldJson : String
voorbeeldJson = 
    """
    {"tag":"div", "attributen": [["class", "container"]], "inhoud": "Hallo, Elm!"}
    """

-- Het decoderen van de voorbeeld JSON naar HtmlGegevens
gedecodeerdeHtmlGegevens : Resultaat Decode.Fout HtmlGegevens
gedecodeerdeHtmlGegevens =
    Decode.decodeerString htmlGegevensDecoder voorbeeldJson

-- Een view renderen van onze HtmlGegevens
weergave : HtmlGegevens -> Html msg
weergave htmlGegevens =
    Html.tekst (htmlGegevens.inhoud)

hoofd : Html msg
hoofd =
    case gecodeerdeHtmlGegevens of
        Ok data ->
            weergave data
        
        Err fout ->
            tekst "Mislukt om HTML-gegevens te parsen"

```

Deze dummy-opstelling laat zien hoe je zou beginnen met het werken met geparseerde HTML-gegevens binnen Elm.

## Diepgaande duik
Historisch gezien betekent Elm's sterke nadruk op typeveiligheid en een solide architectuur dat direct HTML-parsing niet zijn sterke punt is. Elm schittert in het bouwen van betrouwbare webapps met minimale runtimefouten.

Voor HTML-parsing leun je gewoonlijk op JavaScript, dat beschikt over volwassen bibliotheken zoals `DOMParser` en jQuery's `$.parseHTML`. Je doet het zware werk in JavaScript, en stuurt dan de parse tree naar Elm als geserialiseerde JSON. Je kunt hiervoor poorten (Elm's manier van communiceren met JavaScript) of webservices gebruiken.

Eenmaal in Elm, zetten JSON-decoders geserialiseerde gegevens om in gestructureerde Elm-types. Met de JSON-decodeerbenadering geniet je van Elm's typeveiligheid en kun je rommelige HTML-parsinglogica buiten je Elm-codebase houden.

Alternatieven? Als je absoluut binnen Elm HTML moet parsen, zul je waarschijnlijk een aangepaste oplossing nodig hebben. Dit zou het gebruik van een server-side parser die een API blootstelt of een Elm-pakket kunnen zijn—als je er een vindt die aan je behoeften voldoet, hoewel de keuzes momenteel beperkt zijn.

## Zie ook
Voor meer over JSON-decodering in Elm:
- De officiële Elm-gids over JSON: https://guide.elm-lang.org/effects/json.html

Elm-poorten voor JavaScript-interop:
- Elm's gids over poorten: https://guide.elm-lang.org/interop/ports.html

Communitydiscussies en inzichten:
- Elm Discourse: https://discourse.elm-lang.org/
- Elm Slack-kanaal, waar je om hulp kunt vragen en kunt discussiëren: https://elmlang.herokuapp.com/
