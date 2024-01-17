---
title:                "Å jobbe med json"
html_title:           "Gleam: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-json.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Å jobbe med JSON er en vanlig praksis blant programmører for å lagre og overføre data på en strukturert måte. JSON står for JavaScript Object Notation og er et format som tillater enkelt lesbarhet og behandling av data. Det er et populært valg på grunn av dets universalitet og effektiviteten for å kommunisere med applikasjoner og web-tjenester.

Hvordan:

For å arbeide med JSON i Gleam kan du bruke biblioteket gleam/json. Det inkluderer funksjoner for å lese, skrive og manipulere JSON-data. Her er et eksempel på hvordan du kan bruke det til å lese og skrive JSON-data:

```Gleam
let data = "{ "name": "Gleam", "version": "0.9.0" }"
let result = Json.deserialize(data)
// result = Record { name: "Gleam", version: "0.9.0" }

let new_data = Json.serialize(result)
// new_data = "{ "name": "Gleam", "version": "0.9.0" }"
```

Dypere dykk:

JSON har blitt en vanlig måte å lagre og overføre data på siden det ble introdusert i 2001. Den er utledet av JavaScript, men kan brukes med de fleste programmeringsspråk. Det finnes også alternative formater som XML som utfører samme oppgave, men JSON har blitt foretrukket for sin enkelhet og allsidighet. Implementasjonen av JSON i Gleam er basert på Gleam Records, en struktur som er optimal for å representere JSON-data.

Se også:

- Les mer om Gleam Records: https://gleam.run/documentation/standard_library/#record
- Utforsk biblioteket gleam/json: https://github.com/gleam-lang/json
- Finn mer informasjon om JSON på https://www.json.org/