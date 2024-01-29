---
title:                "Debug-output afdrukken"
date:                  2024-01-28T22:04:09.283415-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het printen van debug output laat je waarden uitspugen om te controleren of je programma zich gedraagt zoals verwacht. Het is je basisgereedschap voor snelle en vuile probleemoplossing wanneer dingen fout lopen.

## Hoe te:
```gleam
importeer gleam/io

pub fn main() {
  laat mijn_variabele = "Debuggen in Gleam is eenvoudig!";
  io.debug(mijn_variabele)
}
```
Voer het uit, en je zult `Debuggen in Gleam is eenvoudig!` zien in je terminal. Het toont je wat er op dat moment aan de hand is met je code.

## Dieper Duiken
Historisch gezien gaat het printen van debug output terug naar de dagen dat loggingtools een luxe waren en rubberen eendjes niet op bureaus stonden. Het is het eerste waar een ontwikkelaar zelfs nu aan denkt, ondanks geavanceerde debuggingtools.

In Gleam is `io.debug` de aangewezen weg. Alternatieven zijn onder meer gestructureerde loggingbibliotheken wanneer je simpele printopdrachten ontgroeit. Onder de motorkap schrijft `io.debug` naar standaardfout, waardoor het te onderscheiden is van standaarduitvoer.

## Zie Ook
- De Gleam `io` module documentatie: https://hexdocs.pm/gleam_stdlib/gleam/io/
- Gestructureerd loggen in Gleam: [Module Plaatshouder, gebaseerd op versie-updates]
- Een gids over debuggen in functioneel programmeren: [Link Plaatshouder, afhankelijk van beschikbare bronnen]
