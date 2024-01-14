---
title:    "Elm: Skriving av en tekstfil"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Skriving av en tekstdokumentfil kan være en enkel og effektiv måte å organisere og lagre informasjon på. Tekstfiler kan brukes til å lagre kode, notater, instruksjoner og annen data som du kan ha behov for å referere til senere.

## Hvordan

For å skrive en tekstfil i Elm, kan du bruke funksjonen `File.write`. Her er et eksempel på hvordan du kan bruke denne funksjonen for å opprette og skrive til en ny tekstfil:

```elm
import File
import Text

main =
    File.write "min_dokumentfil.txt" (Text.join "\n" [ "Dette er en tekstfil skrevet i Elm", "Jeg kan legge til så mange linjer som jeg vil", "Denne teksten vil vises i filen" ])
```

I dette eksempelet importeres funksjonen `File.write` fra Elm-biblioteket og `Text.join` som brukes til å kombinere teksten i en liste til en enkeltstreng. Deretter brukes `File.write` til å opprette og skrive til tekstdokumentfilen "min_dokumentfil.txt". Når du åpner denne filen, vil du se at innholdet er det samme som vi har definert i funksjonen.

## Dypdykk

Det finnes ulike måter å skrive og organisere en tekstfil på i Elm. Du kan for eksempel bruke funksjonen `List.map` til å gå gjennom en liste og skrive hver element til sin egen linje i filen. Du kan også bruke funksjonen `Text.append` for å legge til nye linjer i en eksisterende tekstfil.

I tillegg kan du også bruke funksjonen `File.append` for å legge til ny tekst i slutten av en eksisterende fil, eller `File.read` for å lese innholdet i en fil og skrive det til konsollen.

## Se Også

* [Elm Dokumentasjon](https://elm-lang.org/docs)
* [Elm File Pakke](https://package.elm-lang.org/packages/elm/file/latest/File)
* [Elm Text Pakke](https://package.elm-lang.org/packages/elm/text/latest/Text)
* [Elm List Pakke](https://package.elm-lang.org/packages/elm/core/latest/List)