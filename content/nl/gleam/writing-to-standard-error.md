---
title:                "Schrijven naar standaardfout"
date:                  2024-01-28T22:13:12.049485-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (stderr) is als het achterlaten van een spoor van digitale broodkruimels—fouten en waarschuwingen die niet in de reguliere uitvoer thuishoren. Programmeurs gebruiken stderr om problemen te rapporteren zonder de standaarduitvoer (stdout) in de war te sturen, waardoor logs schoon blijven en fouten makkelijker te vinden zijn.

## Hoe:

In Gleam kunnen we naar stderr schrijven met behulp van de `io` module. Laten we direct beginnen:

```gleam
import gleam/io

pub fn main() {
  io.stderr("Oeps! Er is iets fout gegaan.\n") // Stuur een string naar stderr
}
```

Voorbeelduitvoer als dingen misgaan:
```
Oeps! Er is iets fout gegaan.
```

## Uitgebreide Verkenning

- Historisch gezien heeft het categoriseren van uitvoer in stdout en stderr geholpen om reguliere programmaresultaten te scheiden van foutmeldingen.
- Alternatieven zijn onder meer het schrijven van fouten naar een bestand of het gebruik van een logbibliotheek, maar stderr is direct en ligt al binnen handbereik.
- Gleams `io` module handelt stderr af, en spreekt direct de mogelijkheden van de Erlang VM aan.

## Zie Ook

- Het Gleam Boek voor meer over foutafhandeling: [https://gleam.run/book](https://gleam.run/book)
- "The Unix Programming Environment" voor historische achtergrond over Unix's stdout en stderr concepten.
