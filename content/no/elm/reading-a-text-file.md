---
title:                "Lesing av en tekstfil."
html_title:           "Elm: Lesing av en tekstfil."
simple_title:         "Lesing av en tekstfil."
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

"Hva og hvorfor?"

Lesing av tekstfiler er en måte for programmerere å få tilgang til og manipulere data lagret som tekst i et filformat på datamaskinen. Det kan være nyttig for å håndtere store mengder data eller for å håndtere informasjon på en strukturert måte.

"Hvordan:"

```Elm 
import File 

-- Funksjon for å lese og utskrive innholdet i en tekstfil.
File.read "input.txt" 
    |> Task.attempt handleResult

-- Funksjon for å håndtere resultatet og skrive ut teksten.
handleResult : Result File.Error String -> Cmd msg 
handleResult result = 
    case result of 
        -- Hvis filen leses riktig, skriv ut innholdet.
        Ok content -> 
            content 
                |> String.lines
                |> List.map (\line -> 
                    div [] [ text line ]
                    )
                |> div [] 
                
        -- Hvis det oppstår en feil når filen leses, skriv ut feilmelding.
        Err error -> 
            text ("Noe gikk galt: " ++ (File.errorToString error))
```

## Dypdykk:

Å lese tekstfiler har vært en viktig del av programmering siden de første datamaskinene ble utviklet. Det finnes også andre måter å håndtere tekstfiler på, som å bruke en database, men å lese dem direkte er ofte den enkleste og raskeste måten.

Lesing av tekstfiler i Elm er basert på funksjoner fra "File"-modulen, som gjør det enkelt å håndtere selv store filer. Det er også mulig å skrive til og slette tekstfiler ved hjelp av denne modulen.

## Se også:

For mer informasjon om "File"-modulen og hvordan du kan bruke den til å håndtere tekstfiler, kan du sjekke ut Elm sin offisielle dokumentasjon (https://guide.elm-lang.org/).

Du kan også utforske forskjellige funksjoner og muligheter for å lese og håndtere tekstfiler i Elm ved å se på eksempler og bidrag fra andre utviklere på nettsteder som GitHub (https://github.com/elm/file) og Stack Overflow (https://stackoverflow.com/questions/tagged/elm).