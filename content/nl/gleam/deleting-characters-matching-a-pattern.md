---
title:                "Karakters verwijderen die overeenkomen met een patroon"
date:                  2024-01-28T21:58:35.286800-07:00
model:                 gpt-4-0125-preview
simple_title:         "Karakters verwijderen die overeenkomen met een patroon"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van tekens die overeenkomen met een patroon gaat over het vinden van specifieke sequenties in tekst en het verwijderen ervan. Programmeurs doen dit om data te zuiveren, relevante informatie te parsen of input te ontsmetten.

## Hoe doe je dat:
In Gleam werk je meestal met de `String` module voor tekstmanipulatie. Regex is niet ingebouwd, maar je kunt vaste patronen verwijderen of externe bibliotheken gebruiken voor meer complexe taken. Laten we wat tekst opschonen door uitroeptekens uit een string te verwijderen.

```gleam
import gleam/string

pub fn remove_exclamations(text: String) -> String {
  string.replace(text, "!", "")
}

// Gebruik
fn main() {
  let cleaned_text = remove_exclamations("Hello, World!!")
  assert cleaned_text == "Hello, World"
}
```
Deze code vervangt alle uitroeptekens door een lege string, waardoor ze effectief worden verwijderd.

## Uitdieping
Gleam is een statisch getypeerde taal voor de Erlang virtuele machine, met een voorkeur voor prestatie en betrouwbaarheid. Zijn String module biedt basismanipulatiefuncties maar mist geavanceerde patroonafstemming die in regex-bibliotheken wordt gevonden.

Voor historische context, regex bestaat al sinds de jaren 1950, afkomstig uit de formele taaltheorie en automaattheorie. De meeste programmeertalen hebben een vorm van regex-implementatie voor patroonafstemming aangenomen.

In Gleam, om complexere patroonverwijderingen te behandelen, grijp je typisch naar een Erlang bibliotheek of een Elixir module via interoperabiliteit, aangezien het ecosysteem van Gleam nog jong is. De implementatie vertrouwt op de robuustheid van de BEAM (Erlang virtuele machine) en zijn langbestaande bibliotheken.

Alternatieven in Gleam kunnen het schrijven van je eigen patroonafstemmingsfuncties voor meer voorspelbare patronen omvatten, of het behandelen van specifieke gevallen met `String` functies zoals `slice`, `split`, of `trim`.

## Zie ook
- Een introductie tot Regex in het algemeen (niet specifiek voor Gleam): [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
