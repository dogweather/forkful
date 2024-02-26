---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:28.323824-07:00
description: "\xC5 skrive tester i Elm involverer \xE5 lage testtilfeller for \xE5\
  \ verifisere korrektheten av Elm-koden din, og sikre at den oppf\xF8rer seg som\
  \ forventet.\u2026"
lastmod: '2024-02-25T18:49:38.891721-07:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i Elm involverer \xE5 lage testtilfeller for \xE5 verifisere\
  \ korrektheten av Elm-koden din, og sikre at den oppf\xF8rer seg som forventet.\u2026"
title: Skrive tester
---

{{< edit_this_page >}}

## Hva & hvorfor?

Å skrive tester i Elm involverer å lage testtilfeller for å verifisere korrektheten av Elm-koden din, og sikre at den oppfører seg som forventet. Programmerere gjør dette for å fange opp feil tidlig, lette vedlikeholdet og forbedre kvaliteten og påliteligheten til applikasjonene deres.

## Hvordan:

Elm bruker pakken `elm-explorations/test` for å skrive enhets- og fuzz-tester. Start med å legge til pakken i prosjektet ditt:

```elm
elm install elm-explorations/test
```

Opprett en testfil, for eksempel `tests/ExampleTest.elm`, og importer testmodulene. Her er en enkel test som verifiserer en funksjon `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "En enkel legge til-funksjon"
        [ test "Å legge til 2 og 3 gir 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

For å kjøre testene dine, trenger du `elm-test`:

```shell
npm install -g elm-test
elm-test
```

Dette vil kompilere testene dine og skrive ut resultatene i terminalen din. For eksemplet ovenfor, bør utskriften være noe som:

```
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

For et mer komplekst eksempel, la oss si at du vil fuzz-teste `add`-funksjonen for å forsikre deg om at den håndterer et bredt spekter av heltallsinndata korrekt. Du ville modifisere `ExampleTest.elm` som følger:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Tester add med fuzzing"
        [ fuzz int "Fuzz-tester add med tilfeldige heltall" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Kjør `elm-test` igjen for å se fuzz-testene i aksjon. Utdata vil variere med tilfeldige inndata, men vellykkede tester vil indikere ingen feil:

```
TEST RUN PASSED

Duration: 183 ms
Passed:   100
Failed:   0
``` 

Disse eksemplene viser hvordan du skriver og kjører enkle enhets- og fuzz-tester i Elm, ved bruk av pakken `elm-explorations/test`. Testing er en vital del av utviklingsprosessen, som hjelper til med å sikre at Elm-applikasjonene dine er pålitelige og opprettholder høy kvalitet.
