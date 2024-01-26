---
title:                "Arbeid med JSON"
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
JSON (JavaScript Object Notation) er et datautvekslingsformat. Programmerere bruker det fordi det er lettleselig for mennesker og lett å tolke for maskiner.

## Slik gjør du:
Gleam mangler innebygd JSON-støtte, så du må bruke et bibliotek. Her er et eksempel med `gleam/json`:

```gleam
import gleam/json

// Definer en Rust type som matcher JSON strukturen
pub type Cat {
  Cat(name: String, age: Int)
}

// Parse JSON string til en Cat
pub fn decode_cat(json: String) -> Result(Cat, json.Error) {
  case json.decode(json) {
    Ok(cat) -> cat
    Error(err) -> Error(err)
  }
}

// Eksempel på JSON data som en streng
let json_cat = """
{
  "name": "Whiskers",
  "age": 3
}
"""

// Kall til decode_cat funksjonen og print resultat
let result = decode_cat(json_cat)
case result {
  Ok(cat) -> io.println(cat)
  Error(err) -> io.println(flat(err))
}
```

Forventet output vil være en `Cat` instanse eller en feilmelding om parsingen mislykkes.

## Dypdykk
JSON ble popularisert av webutvikling. Alternativer som XML og YAML finns, men JSON er ofte foretrukket for dets enkelhet. I Gleam må JSON-kode manuelt mappes til type-definerte strukturer, i motsetning til språk med automatisk serialisering og deserialisering.

## Se Også
- Gleam JSON bibliotek: [https://hex.pm/packages/gleam_json](https://hex.pm/packages/gleam_json)
- JSON offisiell webside: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- Gleam programmeringsspråk hjemmeside: [https://gleam.run](https://gleam.run)
