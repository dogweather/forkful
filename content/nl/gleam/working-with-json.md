---
title:                "Werken met JSON"
date:                  2024-01-28T22:10:37.642590-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met JSON (JavaScript Object Notation) betekent het omgaan met gegevens in een alomtegenwoordig tekstformaat dat gemakkelijk te begrijpen is voor zowel mensen als machines. Programmeurs doen dit omdat JSON koning is voor het opslaan en overbrengen van gestructureerde gegevens, vooral in webapplicaties.

## Hoe te:

Hier is hoe je met JSON in Gleam kunt werken door gegevens te coderen en te decoderen. Je hebt het `gleam/json` pakket nodig, dus haal dat eerst op.

```gleam
import gleam/json

// Definieer een type
pub type Persoon {
  Persoon(naam: String, leeftijd: Int)
}

// Coderen naar JSON
pub fn encode_persoon(persoon: Persoon) -> json.Json {
  case persoon {
    Persoon(naam, leeftijd) -> 
      json.object([
        "naam", json.string(naam),
        "leeftijd", json.int(leeftijd)
      ])
  }
}
// Gebruik en voorbeelduitvoer
let john = Persoon("John Doe", 30)
let json_john = encode_persoon(john)
json_john // {"naam": "John Doe", "leeftijd": 30}

// Decoderen van JSON
pub fn decode_persoon(json: json.Json) -> Result(Persoon, Nil) {
  let Ok(json) = json.decode_pair() // Decodeer het JSON-object
  let Ok(naam) = json.field("naam").map(json.decode_string)
  let Ok(leeftijd) = json.field("leeftijd").map(json.decode_int)
  persoon.Persoon(naam, leeftijd)
}
// Gebruik en voorbeelduitvoer
let decoded_persoon = decode_persoon(json_object("{\"naam\": \"John Doe\", \"leeftijd\": 30}"))
decoded_persoon // Ok(Persoon("John Doe", 30))
```

## Diepgaande duik

JSON bestaat al sinds de vroege jaren 2000 en vervangt XML in veel scenario's vanwege zijn eenvoud. Alternatieven zijn onder andere YAML, XML en BSON, maar de gebruiksvriendelijkheid van JSON houdt het op de voorgrond. In Gleam leunt de omgang met JSON op pattern matching en de robuuste functies van de `gleam/json` bibliotheek voor een functionele benadering om datastructuren te coderen en te decoderen.

## Zie ook

- De officiële JSON-documentatie van Gleam: [https://hexdocs.pm/gleam_json](https://hexdocs.pm/gleam_json)
- Een inleiding tot JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- De gids over JSON van Mozilla Developer Network: [https://developer.mozilla.org/nl/docs/Learn/JavaScript/Objects/JSON](https://developer.mozilla.org/nl/docs/Learn/JavaScript/Objects/JSON)
