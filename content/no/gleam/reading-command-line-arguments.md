---
title:                "Lese kommandolinjeargumenter"
date:                  2024-01-20T17:56:12.774751-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Lesing av kommandolinjeargumenter lar programmer forstå input gitt når de kjører. Programmerere gjør dette for å la brukerne tilpasse oppførselen til et program.

## How to (Slik gjør du det):
```gleam
import gleam/io
import gleam/list.{first, drop}
import gleam/os.{args}

pub fn main() {
  // Hent argumenter gitt i kommandolinjen
  let arguments = drop(args(), 1) // Fjern det første argumentet, programnavnet
  let user_input = first(arguments)
  
  // En enkel sjekk for å reagere på argumenter
  case user_input {
    Ok("hei") -> io.println("Hei der!")
    Ok("hade") -> io.println("Ha det bra!")
    _ -> io.println("Hallo! Bruk 'hei' eller 'hade'.")
  }
}
```
Kjør ditt program slik: `gleam run hei`.
Forventet utskrift: `Hei der!`

## Deep Dive (Dypdykk):
Før i tiden, benyttet programmer seg av kommandolinjeparametere for nesten all input, ettersom grafiske brukergrensesnitt var sjeldne. Alternativer inkluderer å bruke filinput/output, miljøvariabler, eller interaktiv input.

Implementasjonen av kommandolinjelesing i Gleam er direkte og reflekterer funksjoner funnet i de fleste moderne programmeringsspråk. Gleam er opptatt av typesikkerhet, og bruker derfor 'Result'-typen for å håndtere mulig fraværende argumenter.

## See Also (Se også):
- Erlang's take on command line arguments: [Erlang Command Line Arguments](http://erlang.org/doc/man/init.html)
- Command line parsing strategies: [12 Factor CLI Apps](https://medium.com/@jdxcode/12-factor-cli-apps-dd3c227a0e46)
