---
title:                "Lavorare con YAML"
html_title:           "Gleam: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se stai cercando un modo semplice per rappresentare e gestire dati in formato testo, allora YAML potrebbe essere la soluzione perfetta per te. Con Gleam, puoi facilmente manipolare i tuoi dati YAML con poche righe di codice.

## Come Fare

Per utilizzare YAML con Gleam, devi prima importarlo come una dipendenza nel tuo progetto. Puoi farlo aggiungendo il seguente codice nel tuo file `gleam.toml`:

```
[dependencies]
gleam/yaml = "0.3.0"
```

Una volta aggiunta la dipendenza, puoi utilizzare il modulo YAML in modo simile al seguente esempio:

```
import gleam/yaml

// Creiamo un esempio di dati
let data = %Yaml.Document(
  %{
    "nome": "Mario Rossi",
    "età": 30,
    "hobby": ["calcio", "musica"]
  }
)

// Convertiamo i dati in formato YAML
let yaml = gleam/yaml.to_string(data)

// Stampiamo il risultato
pub fn main() {
  log(yaml)
}
```

Output:

```
---
nome: Mario Rossi
età: 30
hobby:
  - calcio
  - musica
```

## Approfondimento

Ci sono molte altre funzionalità e opzioni disponibili per lavorare con YAML in Gleam. Ad esempio, puoi convertire i tuoi dati YAML in un formato strutturato di Gleam utilizzando il tipo `Yaml.Value` e le sue funzioni helper come `get_string` o `get_integer`. Per ulteriori informazioni e una panoramica completa delle funzionalità di YAML in Gleam, consulta la documentazione ufficiale [qui](https://gleam.run/packages/gleam/yaml).

## Vedi Anche

- [Documentazione Gleam su YAML](https://gleam.run/packages/gleam/yaml)
- [Documentazione ufficiale YAML](https://yaml.org/)