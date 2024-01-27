---
title:                "Manipulation de JSON"
date:                  2024-01-19
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
JSON, c'est du texte pour stocker et échanger des données. Les devs l'utilisent pour parler avec des APIs, sauvegarder des configs, etc.

## Comment faire :
```gleam
import gleam/should
import gleam/json

// Définir une structure
pub type Chat {
  Chat(nom: String, age: Int)
}

// Encoder en JSON
pub fn chat_en_json() {
  let chat = Chat(name: "Felix", age: 3)
  json.encode(chat)
  // Résultat: "{\"nom\":\"Felix\",\"age\":3}"
}

// Décoder du JSON
pub fn json_en_chat() {
  let json_text = "{\"nom\":\"Felix\",\"age\":3}"
  let chat = json.decode(json_text).expect("Décodage valide")
  should.equal(chat, Chat(nom: "Felix", age: 3))
}
```

## Plongeon Profond
Historiquement, XML dominait pour échanger des data; JSON est devenu populaire pour sa simplicité. Alternatives: YAML, TOML. En Gleam, `gleam/json` fournit le nécessaire; c'est efficace, typé.

## Voir Aussi
- JSON.org pour comprendre JSON: [www.json.org/json-fr.html](http://www.json.org/json-fr.html)
- Guide JSON en Gleam: [hexdocs.pm/gleam_json](https://hexdocs.pm/gleam_json)
