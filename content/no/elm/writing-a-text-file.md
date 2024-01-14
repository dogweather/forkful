---
title:                "Elm: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en vanlig oppgave i programmering og kan være nyttig for å lagre data eller konfigurasjonsinnstillinger. Å skrive en tekstfil i Elm er enkelt og kan gjøres ved hjelp av noen få linjer med kode.

## Hvordan

For å skrive en tekstfil i Elm, må du først bruke "File" modulen. Dette kan importeres ved å legge til følgende linje øverst i filen:

```
import File
```

Deretter kan du definere en funksjon for å skrive til filen ved hjelp av ```File.write``` funksjonen. Eksempelvis:

```
writeToFile : String -> Cmd msg
writeToFile content =
    File.write "min_fil.txt" content
```

Her tar funksjonen inn en streng med teksten som skal skrives til filen og sender en "command" for å skrive innholdet til filen "min_fil.txt". Det er viktig å merke seg at dette kun vil fungere i en web-applikasjon, ikke i en vanlig konsoll-applikasjon. 

For å kjøre denne funksjonen, kan vi kalle den fra ```update``` funksjonen i Elm-arkitekturen. For eksempel:

```
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WriteTextToFile text -> 
            ({ model | content = text }, writeToFile text)
```

Her vil funksjonen ```update``` oppdatere modellen med den nye teksten som skal skrives til filen og deretter kalle funksjonen ```writeToFile``` for å faktisk skrive til filen. Merk at vi må definere en ```Msg``` type og en beskjed kalt "WriteTextToFile" for å kunne kalle denne funksjonen. 

## Dypdykk

Når du skriver en tekstfil i Elm, blir filen automatisk opprettet i den samme mappen som din ```elm.json``` fil. Hvis du vil lagre filen et annet sted, må du bruke ```File.FileSystem``` modulen og spesifisere en eksplisitt sti for filen.

Det er også viktig å merke seg at å skrive til en fil er en asynkron prosess, så det kan være nyttig å ha en tilbakemelding til brukeren om at filen ble skrevet til. Dette kan gjøres ved å legge til en ekstra beskjed i ```update``` funksjonen etter at filen er skrevet.

## Se også

- [Offisiell Elm dokumentasjon om å skrive tekstfiler](https://package.elm-lang.org/packages/elm/file/latest/File#write)
- [Enkel Elm tutorial for å skrive tekstfiler](https://guide.elm-lang.org/effects/file.html)
- [Elm discuss post om å skrive til fil i en konsoll-applikasjon](https://discourse.elm-lang.org/t/writing-text-files-in-macos/3586)