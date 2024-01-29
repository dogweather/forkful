---
title:                "Werken met CSV"
date:                  2024-01-28T22:10:10.004912-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met CSV-bestanden (Comma-Separated Values) betekent het omgaan met gegevens in een eenvoudig, op tekst gebaseerd formaat dat tabelgegevens opslaat. Programmeurs gebruiken het omdat het breed ondersteund wordt, makkelijk te lezen is, en eenvoudig te parsen of te genereren is.

## Hoe te:

Momenteel heeft Gleam geen speciale standaardbibliotheek voor CSV-manipulatie, maar je kunt basisparsing implementeren met de ingebouwde functies. Hier is een eenvoudig voorbeeld:

```gleam
import gleam/string

fn parse_csv_line(lijn: String) -> List(String) {
  string.split(lijn, ",")
}

pub fn main() {
  let csv_inhoud = "naam,leeftijd,stad\nAlice,30,New York\nBob,22,Los Angeles"
  let regels = string.split(csv_inhoud, "\n")
  case regels {
    [] -> []
    [.., _header | rijen] -> rijen
      |> list.map(parse_csv_line)
      |> io.debug
  }
}

// Voorbeelduitvoer in de `main` functie:
// [
//   ["Alice", "30", "New York"],
//   ["Bob", "22", "Los Angeles"],
// ]
```
Onthoud om randgevallen zoals komma's in waarden, nieuwe regels en tekstkwalificaties in een volledige implementatie te behandelen.

## Diepgaande Duik

CSV is een oud formaat, daterend uit de beginjaren van de computer, wat bijdraagt aan de brede adoptie ervan. Alternatieven zoals JSON of XML bieden meer structuur maar kunnen complexer zijn om te parsen. Hoe je met CSV-gegevens in Gleam omgaat, kan het gebruik van externe bibliotheken inhouden als die beschikbaar zijn, of het creëren van een aangepaste parser. Serialiseren naar CSV zou zorgvuldig toevoegen van komma's en nieuwe regels kunnen vereisen, en het escapen van noodzakelijke karakters.

## Zie Ook

- Om verschillende bestandsformaten te begrijpen, bekijk de [JSON](https://www.json.org/json-en.html) en [XML](https://www.w3.org/XML/) specificaties.
- Voor complexe CSV-manipulatie, overweeg om bij te dragen aan of gebruik te maken van een CSV-bibliotheek in het [Gleam-ecosysteem](https://hex.pm/) wanneer beschikbaar.
