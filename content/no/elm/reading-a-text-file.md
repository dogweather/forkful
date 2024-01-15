---
title:                "Lesing av en tekstfil"
html_title:           "Elm: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvordan man kan lese og håndtere tekstfiler i Elm? Da bør du definitivt fortsette å lese denne artikkelen! Å kunne lese tekstfiler er en nyttig ferdighet å ha for enhver utvikler, og det kan hjelpe deg med å behandle store mengder data og informasjon på en effektiv måte.

## Slik gjør du det

Først må vi importere et bibliotek kalt "elm/file" som lar oss håndtere filer i Elm. Deretter kan vi bruke funksjoner som "readTextFile" og "writeTextFile" for å lese og skrive innhold til en tekstfil.

```Elm
import File
import Html

file = "tekstfil.txt" -- navnet på filen vi vil lese

-- en funksjon som håndterer teksten som er lest fra filen
handleFileContent : File.Content -> Html msg
handleFileContent content =
  case content of
    File.ReadSuccess fileText -> -- hvis lesingen var vellykket
      -- gjør noe med filteksten, f.eks. skriv den ut på siden
      Html.span [] [Html.text fileText]
    File.ReadFailed _ error -> -- hvis lesingen feilet
      -- håndter feilen på en eller annen måte
      Html.div [] [Html.text "Lesing mislyktes"]

-- leser tekstfilen og håndterer innholdet
File.readTextFile file handleFileContent
```

Kjører du denne koden vil du se at innholdet fra tekstfilen "tekstfil.txt" vises på skjermen.

## Dypdykk

Når vi bruker funksjonen "readTextFile", blir filinnholdet lest og returnert som en "File.Content" type. Dette kan enten være "File.ReadSuccess" eller "File.ReadFailed" avhengig av om lesingen var vellykket eller ikke. Dette betyr at vi må håndtere begge disse tilfellene når vi skriver koden vår.

En annen ting å merke seg er at vi ikke kan lese filer lokalt i nettleseren. Vi må enten bruke en server eller en plugin for å kunne gjøre dette, som for eksempel "elm/file-reader". Det er også viktig å huske på at filstørrelsesbegrensningene for nettlesere fortsatt gjelder, så det kan hende du må håndtere store filer i deler.

## Se også

- Elm filbibliotek: https://package.elm-lang.org/packages/elm/file/latest/
- Elm fil-leser plugin: https://package.elm-lang.org/packages/gampleman/elm-file-reader/latest/