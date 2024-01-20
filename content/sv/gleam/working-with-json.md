---
title:                "Arbeta med JSON"
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jobbar med JSON hanterar datastrukturer i textformat. Programmerare använder det för att enkelt utbyta data mellan olika system.

## Hur gör man:
```gleam
import gleam/should
import gleam/json

pub fn main() {
  let data = json.from_string("{\"name\":\"Olle\",\"age\":30}")
  let result = case data {
    Ok(value) -> value
    Error(_) -> json.null
  }
  should.equal(result, json.Object(map.from_list([#("name", json.String("Olle")), #("age", json.Number(30.0))])))
}
```
Utskrift:
```
Ok(#{"age": 30, "name": "Olle"})
```

## Fördjupning
JSON, JavaScript Object Notation, introducerades tidigt 2000-tal. Alternativ inkluderar XML, YAML. JSON är mindre krångligt och lätt att implementera, stöds av de flesta programmeringsspråk.

## Se även
- [JSON specification](https://www.json.org/json-en.html)
- [Gleam's official book](https://gleam.run/book/)