---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML er et dataformat som er lett å lese for mennesker, og brukes ofte for konfigurasjonsfiler. Programmerere bruker YAML for å organisere data og innstillinger på en enkel og oversiktlig måte.

## How to:
For å jobbe med YAML i Go, bruk `go-yaml` pakken. Her er et enkelt eksempel på hvordan parse en YAML-streng:

```Go
package main

import (
    "fmt"
    "log"

    "gopkg.in/yaml.v3"
)

type Config struct {
    Hovedstilling string `yaml:"hovedstilling"`
    Erfaring int `yaml:"erfaring"`
}

func main() {
    ymlData := `
hovedstilling: "Utvikler"
erfaring: 5
`
    var config Config

    err := yaml.Unmarshal([]byte(ymlData), &config)
    if err != nil {
        log.Fatalf("error: %v", err)
    }

    fmt.Printf("Stilling: %s\nErfaring: %d år\n", config.Hovedstilling, config.Erfaring)
}
```

Kjøring vil gi dette resultatet:

```
Stilling: Utvikler
Erfaring: 5 år
```

## Deep Dive
YAML ("YAML Ain't Markup Language") ble først lansert i 2001. Det tillater komplekse datastrukturer å representeres på en menneskelesbar måte. Alternativer til YAML inkluderer JSON og XML, men YAML er ofte foretrukket for sin lesbarhet. Internt bruker Go-`yaml` pakken refleksjon for å tilordne YAML-data til Go-strukturer, noe som gjør det enkelt å arbeide med tilpassede datatyper.

## See Also
- YAML spesifikasjonen: https://yaml.org/spec/1.2/spec.html
- `go-yaml` GitHub repo: https://github.com/go-yaml/yaml
- Go-dokumentasjon for pakken refleksjon: https://pkg.go.dev/reflect
