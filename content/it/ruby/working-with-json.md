---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, acronimo di JavaScript Object Notation, è un formato leggero per lo scambio di dati, facilmente leggibile sia per gli esseri umani che per le macchine. I programmatori lo usano soprattutto per trasferire dati tra server e applicazioni web, o per salvare configurazioni in modo semplice ed efficace.

## How to:
```Ruby
require 'json'

# Creare un hash e convertirlo in JSON
utente = { nome: "Mario", professione: "Sviluppatore", eta: 30 }
utente_json = utente.to_json
puts utente_json
# Output: {"nome":"Mario","professione":"Sviluppatore","eta":30}

# Leggere JSON e convertirlo in un hash in Ruby
json_ricevuto = '{"nome":"Luigi","professione":"Grafico","eta":25}'
hash_ricevuto = JSON.parse(json_ricevuto)
puts hash_ricevuto["nome"]
# Output: Luigi
```

## Deep Dive
JSON è nato nei primi anni 2000 come alternativa a XML, più verboso e complesso. Paragonato a YAML, altro formato di serializzazione dei dati, JSON è più rigoroso nella sintassi ma supportato in modo nativo da JavaScript, il che ne ha garantito una rapida adozione negli ambienti web. In Ruby, il modulo `json` è incluso dalla versione 1.9, facilitando l'encoding e decoding di oggetti JSON. Attenzione a gestire eccezioni come `JSON::ParserError` quando si elaborano dati JSON potenzialmente non validi.

## See Also
- La documentazione ufficiale di JSON in Ruby: [ruby-doc.org](https://ruby-doc.org/stdlib-3.0.0/libdoc/json/rdoc/JSON.html)
- Una guida su come usare JSON in Rails: [guides.rubyonrails.org](https://guides.rubyonrails.org/active_support_core_extensions.html#json-support)
- Specifiche del formato JSON: [json.org](https://www.json.org/json-it.html)
