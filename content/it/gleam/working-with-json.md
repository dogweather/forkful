---
title:                "Lavorare con JSON"
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON è apparso come stenografia per JavaScript Object Notation. È un formato di dati leggibile per l'interoperabilità dei dati. I programmatori lo usano per lo scambio dati tra server e applicazioni web, essendo facile da comprendere e veloce da analizzare.

## How to:
Con Gleam, utilizzare JSON è diretto. Mettiamo che vogliamo serializzare un record in JSON e deserializzarlo.

```Gleam
import gleam/json

type Persona {
  Persona(name: String, age: Int)
}

// Serializzazione
pub fn persona_to_json(persona: Persona) -> json.Value {
  case persona {
    Persona(name, age) -> json.object([
      "name", json.string(name),
      "age", json.int(age)
    ])
  }
}

// Deserializzazione
pub fn json_to_persona(json: json.Value) -> Result(Persona, String) {
  json
  |> json.map(field: {
    "name", json.string,
    "age", json.int,
  })
  |> result.map(fn(fields) {
    Persona(fields.name, fields.age)
  })
}

// Esempio di utilizzo
fn main() {
  let mia_persona = Persona("Mario", 30)
  mia_persona
  |> persona_to_json
  |> json.encode  // "{\"name\":\"Mario\",\"age\":30}"
  
  // Ritorno alla struttura Persona
  let json_persona = "{\"name\":\"Mario\",\"age\":30}"
  json_persona
  |> json.decode
  |> result.then(json_to_persona)
}
```

## Deep Dive
JSON è nato nei primi anni 2000, pensato come una parte di JavaScript ma poi adottato oltre. Alternativa famosa è XML, ma JSON è più leggero e di facile lettura. La libreria `gleam/json` facilita la serializzazione e deserializzazione con funzioni dedicate. JSON non ha tipi specifici per data o ora, quindi è necessario gestirli come stringhe.

## See Also
- Documentazione ufficiale di Gleam: [Gleam](https://gleam.run)
- JSON homepage: [JSON.org](https://json.org)
- Comparazione tra JSON e XML: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
