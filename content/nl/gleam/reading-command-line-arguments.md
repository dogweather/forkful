---
title:                "Commandoregelargumenten lezen"
date:                  2024-01-28T22:05:21.023288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Commandoregelargumenten lezen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het lezen van commandoregelargumenten stelt programma's in staat om te handelen op basis van gegevens die tijdens het opstarten worden doorgegeven. Programmeurs gebruiken het om het gedrag aan te passen zonder de code te wijzigen.

## Hoe:

De `main` functie van Gleam kan toegang krijgen tot commandoregelargumenten via een lijst van strings. Itereer, pattern-match en doe wat nodig is.

```gleam
import gleam/io

fn main(args: List(String)) {
  let bericht = match args {
    [] -> 
      "Geen argumenten gevonden."
    [enkel] -> 
      enkel
    _ -> 
      "Te veel argumenten!"
  }
  io.println(bericht)
}
```

Het programma uitvoeren:

```
$ mijn_programma
Geen argumenten gevonden.

$ mijn_programma "Hallo, Gleam!"
Hallo, Gleam!

$ mijn_programma Te veel argumenten gegeven
Te veel argumenten!
```

## Diepere Duik

Het lezen van commandoregelargumenten is een basiscomponent in programmeren sinds de vroege dagen. UNIX-tools blinken hierin uit. Gleam, geworteld in de Erlang VM, biedt een moderne aanraking aan deze functionaliteit. Alternatieven omvatten parsingbibliotheken voor complexe gevallen, zoals optievlaggen. Gleam doet dit zonder de wijdlopigheid van Erlang of de obscuriteit van C.

## Zie Ook

Voor verdere verkenning:

- De officiële documentatie van Gleam: https://gleam.run/book
- Erlang's `escript`: http://erlang.org/doc/man/escript.html
- Commandoregel-parsingbibliotheken: Zie Gleams pakketrepository op https://hex.pm/packages?search=gleam
