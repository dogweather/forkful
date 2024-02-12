---
title:                "Lavorare con YAML"
aliases:
- /it/go/working-with-yaml.md
date:                  2024-02-03T18:13:41.469016-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e perché?

Lavorare con YAML in Go implica l'analisi di file YAML (YAML Ain't Markup Language), uno standard di serializzazione di dati amichevole per l'utente, nelle strutture dati di Go e viceversa. I programmatori lo fanno per sfruttare la semplicità e leggibilità di YAML per i file di configurazione, le impostazioni delle applicazioni o lo scambio di dati tra servizi e componenti scritti in linguaggi diversi.

## Come fare:

Per lavorare con YAML in Go, dovrai prima importare una libreria che supporti l'analisi e la serializzazione di YAML, poiché la libreria standard di Go non include supporto diretto per YAML. La libreria più popolare per questo scopo è "gopkg.in/yaml.v3". Ecco come iniziare:

1. **Installazione del pacchetto YAML:**

```bash
go get gopkg.in/yaml.v3
```

2. **Analisi di YAML in una struct Go:**

Prima, definisci una struct in Go che corrisponda alla struttura dei tuoi dati YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: segreto
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("errore: %v", err)
  }
  fmt.Printf("Utente: %s\nPassword: %s\n", config.Database.User, config.Database.Password)
}
```

**Esempio di output:**

```
Utente: admin
Password: segreto
```

3. **Serializzazione di una struct Go in YAML:**

Ecco come convertire una struct Go di nuovo in YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      Utente:     "admin",
      Password: "supersegreto",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("errore: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**Esempio di output:**

```yaml
---
database:
  user: admin
  password: supersegreto
```

## Approfondimento:

L'uso di YAML nello sviluppo del software è cresciuto a causa del suo formato leggibile dall'uomo, rendendolo una scelta ideale per file di configurazione, documentazione o formati di scambio dati. Rispetto a JSON, il suo omologo, YAML offre commenti, tipi scalari e funzionalità di relazione, fornendo un framework di serializzazione dei dati più ricco. Tuttavia, la sua flessibilità e caratteristiche comportano una complessità nell'analisi, portando a potenziali rischi per la sicurezza se non gestiti con cura (ad es., esecuzione di codice arbitrario).

La libreria "gopkg.in/yaml.v3" per Go è una soluzione robusta per l'elaborazione di YAML, trovando un equilibrio tra facilità d'uso e supporto completo delle funzionalità. Allo stato attuale, sebbene ci siano alternative come "go-yaml/yaml" (la libreria dietro "gopkg.in/yaml.v3"), la versione scelta dipende di solito da requisiti specifici del progetto o preferenze personali. Quando si tratta di set di dati massicci o applicazioni critiche per le prestazioni, i programmatori potrebbero considerare formati più semplici come JSON per il loro minor tempo di analisi e sovraccarico di memoria. Tuttavia, per file di configurazione o impostazioni in cui la leggibilità umana e la facilità d'uso sono di primaria importanza, YAML rimane un forte contendente nell'ecosistema Go.
