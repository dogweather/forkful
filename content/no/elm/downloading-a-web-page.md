---
title:                "Nedlasting av en nettside"
html_title:           "Elm: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Nedlasting av en nettside betyr rett og slett å hente informasjonen som er lagret på den siden og vise den på din egen enhet. Programmere gjør dette for å hente og behandle data, for eksempel å vise informasjon på en nettside eller lagre den for senere bruk.

## Hvordan:

```Elm
import Html exposing (text)
import Http

-- URLen til nettsiden vi skal laste ned
url = "https://www.eksempel.com"

-- Vi bruker funksjonen send for å sende en GET forespørsel til urlen
request = Http.send (Http.get url)

-- Når responsen kommer tilbake, kan vi bruke en decoder for å hente ut ønsket data
response = Html.text "Response body:" ++ (Http.expectString Response)

-- Vi kan også håndtere eventuelle feilmeldinger dersom noe går galt under nedlastingen
onError = Html.text "Something went wrong..."

-- Så slår vi disse tre funksjonene sammen ved hjelp av en Http task
task = Http.Task.andThen onGotUrl response onError request

-- Og kjører tasken med en renderer funksjon som vil vise resultatet
main = Html.beginnerProgram { view = task, model = (), update = always () }
```

Output:
```
Response body: <HTML> ... </HTML>
```

## Dypdykk:

Nedlasting av nettsider har vært en viktig del av webutvikling i lang tid, men det er stadig enklere og mer effektivt å gjøre med moderne programmeringsspråk som Elm. Her bruker vi en kombinasjon av Html og Http pakker for å gjøre denne oppgaven.

Alternativer til Elm inkluderer JavaScript og andre språk som også kan gjøre dette. Imidlertid er Elm kjent for sin strenge typesikkerhet og klare syntax, som gjør det til et populært valg for mange utviklere.

Det er også verdt å merke seg at denne nedlastingsmetoden kun fungerer for å hente informasjon fra sider som ikke krever pålogging eller autentisering.

## Se også:

Offisiell Dokumentasjon:
https://package.elm-lang.org/packages/elm-lang/http/latest/

Elm pakker for å jobbe med HTML:
https://package.elm-lang.org/packages/elm-lang/html/latest/