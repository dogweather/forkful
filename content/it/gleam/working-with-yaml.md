---
title:                "Lavorare con YAML"
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un linguaggio di serializzazione dati leggibile, usato per configurazione, automazione e integrazione tra servizi. I programmatori lo usano per la sua chiarezza e facilità di utilizzo.

## How to:
Gleam attualmente non ha librerie standard per il parsing di YAML. Di seguito un esempio usando una libreria fittizia:

```gleam
import yaml

pub fn main() {
  let config = "nome: Mario Rossi\netà: 30"
  let dati = yaml.parse(config)
  case dati {
    Ok(d) -> io.println(d)
    Error(e) -> io.println("Errore nel parsing del YAML: " ++ e)
  }
}
```
Output:
```
Ok(#{"nome": "Mario Rossi", "età": 30})
```

## Deep Dive
YAML, che sta per "YAML Ain't Markup Language", è nato nel 2001. Nonostante JSON e TOML siano alternative popolari, YAML rimane scelta comune per la sua leggibilità. In Gleam, lavorare con YAML normalmente richiederebbe una libreria esterna fornita dalla comunità o l'interoperabilità con codice Erlang o Elixir.

## See Also
- YAML specifica ufficiale: [yaml.org](https://yaml.org)
- JSON e TOML come alternative: [json.org](https://json.org), [toml.io](https://toml.io)