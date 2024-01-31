---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con YAML significa manipolare dati in "YAML Ain't Markup Language", un formato di serializzazione umanamente leggibile. I programmatori lo usano perché è intuitivo, compatibile con diversi linguaggi di programmazione e adatto per configurazioni, file di dati e lo scambio di messaggi tra servizi.

## How to:
Per gestire YAML in Go, puoi utilizzare il pacchetto `go-yaml`. Installalo con `go get gopkg.in/yaml.v3`. Ecco un esempio di codice per leggere e scrivere YAML:

```Go
package main

import (
    "fmt"
    "log"
    "gopkg.in/yaml.v3"
)

// Struttura di esempio per i nostri dati YAML
type Config struct {
    Version string `yaml:"version"`
    Services map[string]Service `yaml:"services"`
}

type Service struct {
    Image string `yaml:"image"`
    Ports []string `yaml:"ports"`
}

func main() {
    // YAML di esempio
    data := `
version: "3"
services:
  webapp:
    image: "example/webapp"
    ports:
      - "5000:5000"
`
    // Inizializza una nuova istanza di Config
    var config Config

    // Unmarshal analizza il YAML in input e lo riempie nella struttura di Config
    err := yaml.Unmarshal([]byte(data), &config)
    if err != nil {
        log.Fatalf("Errore durante l'unmarshal: %v", err)
    }
    
    fmt.Println(config.Version) // Stampa la versione
    fmt.Println(config.Services["webapp"].Image) // Stampa l'immagine del servizio webapp

    // Modifica i valori 
    config.Services["webapp"].Image = "example/newwebapp"

    // Marshal ritorna la nuova configurazione YAML
    newData, err := yaml.Marshal(&config)
    if err != nil {
        log.Fatalf("Errore durante il marshal: %v", err)
    }
    
    fmt.Println(string(newData)) // Stampa il nuovo YAML
}
```
Output:
```
"3"
"example/webapp"
version: "3"
services:
  webapp:
    image: example/newwebapp
    ports:
    - 5000:5000
```

## Deep Dive:
YAML è stato creato nel 2001 ed è un superset di JSON, fornendo una maggiore leggibilità. Alternativamente, potresti lavorare con JSON o XML, ma YAML è favorito per la sua chiarezza. A livello di implementazione, Go gestisce YAML usando la reflection per mappare i valori dai dati YAML alle strutture Go. Attenzione all'indentazione: è essenziale in YAML e richiede precisione.

## See Also:
- Documentazione ufficiale YAML: https://yaml.org
- Go package `go-yaml`: https://pkg.go.dev/gopkg.in/yaml.v3
- Tutorial YAML per Go: https://www.sohamkamani.com/golang/yaml/
