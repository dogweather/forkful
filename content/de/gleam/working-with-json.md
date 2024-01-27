---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?

JSON ist ein Datenformat, das für den Datenaustausch zwischen Systemen verwendet wird. Programmierer nutzen es, weil es leichtgewichtig, menschenlesbar und einfach zu parsen ist.

## Wie:

```gleam
import gleam/json
import gleam/dynamic.{from_dynamic, Dynamic}
import gleam/should

type User {
  User(name: String, age: Int)
}

pub fn main() -> Result(User, String) {
  // JSON-String in Gleam
  let json_string = "{\"name\":\"Max\",\"age\":25}"
  
  // Parsen des JSON-Strings in eine dynamische Gleam-Struktur
  let dynamic_data = json.decode(json_string)

  // Konvertieren der Dynamik in einen User-Typ
  dynamic_data
  |> from_dynamic(
    fn(d) {
      let name = d.field("name")?.to_string()
      let age = d.field("age")?.to_int()
      Ok(User(name, age))
    }
  )
}

// Beispielhafter Output eines Testfalls
fn suggest_name_test() {
  let user = User("Max", 25)
  let suggestion = suggest_new_name(user, "Moritz")

  should.equal(suggestion, Ok(User("Moritz", 25)))
}
```

## Tiefgang

JSON begann als Teil von JavaScript, hat aber schnell Sprachbarrieren überwunden. Formate wie XML wurden früher genutzt, bieten aber nicht die gleiche Einfachheit wie JSON. In Gleam gibt es Bibliotheken wie `gleam/json`, welche Parsing und Serialisierung übernehmen. Alles ist typsicher – ein großer Vorteil gegenüber schwach typisierten Sprachen.

## Siehe Auch

- Offizielle JSON-Webseite: [json.org](https://www.json.org/json-de.html)
- Gleam JSON-Bibliotheksdokumentation: [hex.pm gleam/json](https://hex.pm/packages/gleam_json)
- JSON und Typsicherheit in Gleam: [github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
