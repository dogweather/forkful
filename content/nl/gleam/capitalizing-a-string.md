---
title:                "Een string met hoofdletters maken"
date:                  2024-01-28T21:55:53.818488-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een string kapitaliseren betekent de eerste letter in hoofdletters zetten terwijl de rest kleine letters blijft. Programmeurs doen dit om data-invoer te standaardiseren, voor stilistische doeleinden, of om te voldoen aan grammaticale regels in gebruikersinterfaces.

## Hoe te:

In Gleam kunnen we een functie definiëren om een string te kapitaliseren. Momenteel biedt de standaardbibliotheek van Gleam niet rechtstreeks een kapitaliseerfunctie, dus we maken er zelf een met behulp van string slicing:

```gleam
import gleam/string

pub fn capitalize(text: String) -> String {
  let head = string.slice(text, 0, 1)
  let tail = string.slice(text, 1, string.len(text))
  
  string.append(string.uppercase(head), string.lowercase(tail))
}

pub fn main() {
  assert capitalize("hello") == "Hello"
  assert capitalize("WORLD") == "World"
}
```

Voorbeelduitvoer:

```plaintext
"Hello"
"World"
```

## Diepgaande Verkenning

Historisch gezien is de functie om strings te kapitaliseren vaak opgenomen in de standaardbibliotheken van veel talen. Echter, Gleam, als een jonge taal, mist misschien bepaalde handige functies, waardoor het aan ontwikkelaars overgelaten wordt om deze te implementeren. Alternatieven voor het kapitaliseren van strings kunnen het gebruik van reguliere expressies of unicode-bibliotheken omvatten voor complexere gevallen die taalspecifieke regels overwegen. In ons basisvoorbeeld zijn de implementatiedetails eenvoudig: we verdelen de string in twee delen (kop en staart), kapitaliseren de eerste letter en voegen ze weer samen.

## Zie Ook

- Unicode-tekstsegmentatie voor complexere regels: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)
