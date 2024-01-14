---
title:                "Elm: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor?

Hvis du er interessert i Elm-programmering, vil du kanskje lure på hvorfor du bør lese en tekstfil. Å lese en tekstfil er et viktig konsept innenfor programmering, og det kan gi deg en bedre forståelse av hvordan du kan håndtere og behandle ulike typer data.

## Slik gjør du det

For å lese en tekstfil i Elm, kan du bruke funksjonen `Text.fromFile` som tar inn en filbane som argument. Her er et eksempel på hvordan du kan bruke denne funksjonen for å lese en fil med navnet "mitttekstdokument.txt" og vise innholdet i konsollen:

```Elm
main =
  Text.fromFile "mitttekstdokument.txt"
    |> Task.attempt OnFileRead

type Msg
  = OnFileRead (Result File.Error Text.Text)

update msg model =
  case msg of
    OnFileRead (Ok text) ->
      ( text, Cmd.none )
    OnFileRead (Err err) ->
      ( String.fromInt err, Cmd.none )

view content =
  text content
```

Når du kjører koden over, vil du få følgende resultat i konsollen:

> "Dette er innholdet i min tekstfil."

Husk at det er viktig å håndtere feil når du leser en fil, derfor bruker vi en `Case`-uttrykk for å sjekke om operasjonen var vellykket eller ikke.

## Dykk dypere

Det finnes flere måter å lese og behandle en tekstfil på i Elm, avhengig av hvilken type data du jobber med. Du kan for eksempel bruke funksjonen `Text.lines` for å lese hver linje i en tekstfil som en liste av tekststrenger, eller `Text.split` for å dele opp teksten basert på et bestemt separator-tegn.

Det er også viktig å være oppmerksom på at filbehandling er en asynkron operasjon, noe som betyr at du må sørge for at koden din håndterer dette riktig ved hjelp av funksjoner som `Task`, `Cmd` eller `Msg`.

## Se også

- Elm dokumentasjon om `Text`-modulen: https://package.elm-lang.org/packages/elm/core/latest/Text
- How to use the Elm Text.fromFile Function: https://thoughtbot.com/blog/how-to-use-the-elm-text-from-file-function