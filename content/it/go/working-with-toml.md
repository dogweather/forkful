---
title:                "Lavorare con TOML"
date:                  2024-01-26T04:22:21.445846-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-toml.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Lavorare con TOML comporta l'analisi e la codifica di file TOML (Tom's Obvious, Minimal Language) in Go. I programmatori optano per TOML per la sua leggibilità e facile mappatura alle strutture dati, una solida scelta per le configurazioni.

## Come fare:
Per lavorare con TOML in Go, si utilizza tipicamente una libreria come `BurntSushi/toml`. Ecco uno sguardo rapido all'analisi di un file di configurazione TOML:

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

Esempio di `config.toml`:

```Toml
title = "Esempio TOML"
[owner]
name = "Tom Preston-Werner"
```

Esempio di output:

```
Title: Esempio TOML, Owner: Tom Preston-Werner
```

## Approfondimento
TOML, introdotto da Tom Preston-Werner nel 2013, è stato progettato per essere un formato di file di configurazione minimale che è facile da leggere grazie alla sua semantica chiara. Gli sviluppatori Go utilizzano spesso TOML per la configurazione rispetto ad alternative come JSON o YAML per la sua semplicità e capacità di rappresentare gerarchie complesse con facilità.

Rispetto a YAML, che ha funzionalità complesse e potenziali problemi di sicurezza, il design piatto di TOML riduce la complessità e gli errori dovuti a refusi. E a differenza di JSON, TOML supporta i commenti, rendendo più facile spiegare le configurazioni in-linea.

Quando si lavora con TOML in Go, ci sono sfumature da considerare. I tag struct possono personalizzare come le tue struct si mappano alle strutture TOML, e si dovrebbe anche essere consapevoli di come gli array TOML e le tabelle in linea vengono analizzati in slice e mappe Go.

## Vedi Anche
- Specifica TOML: https://toml.io/en/
- Libreria BurntSushi/toml: https://github.com/BurntSushi/toml
- Un confronto dei formati di file di configurazione: https://www.redhat.com/sysadmin/yaml-toml-json-differences
