---
title:                "Analysera html"
html_title:           "Elm: Analysera html"
simple_title:         "Analysera html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin undrat hur din internetbrowser vet vad den ska visa på skärmen när du besöker en websida? En av nyckelkomponenterna är parsing av HTML, som översätter webbsidans kod till något som din dator förstår. I denna artikel kommer vi att undersöka hur man gör detta med hjälp av Elm-programmering.

## Hur man gör det

För att parsar HTML i Elm, behöver vi först importera paketet 'elm/html' och inkludera dess funktioner i vår kod. Sedan kan vi definiera vår HTML struktur med hjälp av 'div', 'p', 'span' och andra taggar. När vi är nöjda med vår layout, använder vi funktionen 'Html.map' för att konvertera vår HTML till en sträng för att kunna använda det på vår webbsida.

```Elm
import Html exposing (..)
import Html.Attributes exposing (..)

-- definiera en HTML struktur
myHtml =
    div []
        [ p [ class "title" ] [ text "Välkommen!" ]
        , span [] [ text "Detta är en Elm artikel." ]
        ]

-- konvertera till sträng och använda på vår sida
main =
    myHtml
        |> Html.map toString
```

Om vi kör denna kod kommer vi att se följande utmatning på vår webbsida:

```html
<div><p class="title">Välkommen!</p><span>Detta är en Elm artikel.</span></div>
```

## Deep Dive

När vi parsar HTML med Elm, finns det några användbara funktioner som vi kan använda för att manipulera vår HTML struktur. Till exempel, 'Html.map' som vi använde tidigare, låter oss konvertera vår struktur till en sträng. 'Html.map' kan också användas för att mappla över vår struktur och applicera olika transformationer på våra element. Dessutom kan vi också använda funktionen 'Html.Keyed.map' för att identifiera och uppdatera specifika element i vår HTML.

En annan viktig aspekt av HTML parsing är att hantera attribut. I Elm, använder vi 'Html.Attributes' för att lägga till klass, id och andra attribut till våra element. Vi kan även använda CSS för att styla våra HTML-element genom att använda 'Html.Attributes.style'.

## Se även

- Elm Dokumentation: https://guide.elm-lang.org/
- Elm Architecture: https://guide.elm-lang.org/architecture/