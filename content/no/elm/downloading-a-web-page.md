---
title:                "Elm: Last ned en nettside"
simple_title:         "Last ned en nettside"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å laste ned en nettside kan være nyttig for å kunne jobbe med og analysere nettsider selv uten å være koblet til internett. Det kan også være en god måte å lære om hvordan nettsider fungerer og interagerer med data.

## Hvordan å

Vi kan bruke det funksjonelle programmeringsspråket Elm for å enkelt laste ned en nettside. Først må vi importere et modul som hjelper oss med å hente data fra en URL. Deretter kan vi bruke funksjonen `Http.getString` og gi den en nettsideadresse, som vil returnere en `Task` som inneholder nettsiden sin HTML-kode.

Et eksempel på hvordan dette kan se ut i Elm-kode:

```elm
import Http

getWebPage : String -> Task Http.Error String
getWebPage url =
    Http.getString url

-- Eksempel på å hente HTML-koden til Google sine søkesider
googleSearchPage : Task Http.Error String
googleSearchPage =
    getWebPage "https://www.google.com/search?source=hp&ei=xxxx"

```

Når vi kjører koden, vil HTML-koden til Google sin søkeside bli lagret i en `Task` som vi kan bruke og jobbe med.

## Dypdykk

Det er viktig å merke seg at denne metoden vil returnere HTML-koden til nettsiden slik den ser ut på det aktuelle tidspunktet du laster den ned. Dette betyr at hvis nettsiden blir endret, vil koden du har lastet ned fortsatt være den gamle versjonen.

Det er også viktig å være bevisst på at det å laste ned en nettside kan være en ressurskrevende prosess, spesielt for større nettsider med mye innhold. Det er derfor viktig å være forsiktig med å bruke denne metoden for å unngå å overbelaste nettleseren din.

## Se også

- Elm sin offisielle nettside: https://elm-lang.org/
- Elm sin dokumentasjon om HTTP-modulen: https://package.elm-lang.org/packages/elm/http/latest/
- En guide om hvordan du kan jobbe med og analysere HTML med Elm: https://www.brianthicks.com/post/2021-01-05-working-with-html-in-elm/