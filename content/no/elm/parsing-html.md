---
title:                "Analysering av HTML"
html_title:           "Elm: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å analysere HTML (Hypertext Markup Language) betyr å ta en tekst som beskriver et websideformat og gjøre om det til et leselig og manipulerbart format for datamaskiner. Programmører gjør dette for å kunne hente ut og bearbeide informasjon fra nettsider på en effektiv måte.

## Slik gjør du:

```Elm
import Html
import Html.Attributes

parsetHtml : String -> Html msg
parsetHtml htmlString =
    let
        document = htmlString |> Html.parse
    in
        case document of 
            Ok doc -> 
                Html.div [] [ Html.text (Html.nodeToHtml doc) ]
            Err erroMsg -> 
                Html.div [] [ Html.text erroMsg ]
```

Merknader: For å analysere HTML i Elm, må du importere `Html` og `Html.Attributes` bibliotekene. Deretter kan du bruke funksjonen `parse` for å konvertere en streng av HTML-kode til et `document`-objekt. Ved å bruke `nodeToHtml` funksjonen, kan du få ut den konverterte HTML-koden og plassere den inn i et element, som i eksempelet over et `div`-element. 

Eksempeloutput:

```Elm
<div> 
    <h1>Tittel</h1>
    <p>Dette er en paragraf.</p>
</div>
```

## Detaljert informasjon:

HTML-analyse har vært en viktig del av nettutvikling siden starten av weben på 1990-tallet. Det har blitt gjort på forskjellige måter, for eksempel ved å bruke regex (regular expressions). Men med nye programmeringsspråk som Elm, kan dette gjøres enklere og mer pålitelig ved hjelp av funksjonsprogrammering.

Et alternativ til Elm for å analysere HTML er å bruke JavaScript, som også er mye brukt for frontend-utvikling. Men i motsetning til JavaScript, som er et multi-paradigme språk, er Elm spesielt designet for å håndtere funksjonell programmering, noe som gjør det til et kraftig verktøy for HTML-analyse.

En interesant detalj ved Elm sin `parse`-funksjon er at den bruker et konvertibelt bibliotek som heter "elm-parser". Med dette biblioteket kan `parse` gjøre om tekst til data på en generisk måte, slik at det kan brukes til å konvertere andre formater i tillegg til HTML.

## Se også:

- Elm's offisielle dokumentasjon for HTML-analyse: https://package.elm-lang.org/packages/elm/html/latest/Html
- En detaljert guide om Elm sin HTML-analysefunksjon: https://medium.com/@DevProgress/parse-html-in-elm-using-elm-parser-3e496e3bdd50