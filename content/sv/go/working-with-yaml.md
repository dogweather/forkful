---
title:                "Arbete med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett format för data-serialisering, lättläst för människor. Programmerare använder det för konfigurationer och datalagring eftersom det är enkelt och tydligt.

## Hur man gör:
För att hantera YAML i Go, använd paketet `gopkg.in/yaml.v2` eller `gopkg.in/yaml.v3`. Exempelkod:

```Go
package main

import (
    "fmt"
    "log"
    "gopkg.in/yaml.v2"
)

type Config struct {
    Title  string
    Owner  struct {
        Name string
    }
}

func main() {
    data := `
title: Example YAML
owner:
  name: Jan Andersson
`
    var config Config
    
    err := yaml.Unmarshal([]byte(data), &config)
    if err != nil {
        log.Fatalf("error: %v", err)
    }
    fmt.Printf("--- config:\n%v\n", config)
}
```

När du kör programmet får du:

```
--- config:
{Example YAML {Jan Andersson}}
```

## Fördjupning
YAML, som står för "YAML Ain't Markup Language", började användas i början av 2000-talet som ett enklare alternativ till XML. Alternativ inkluderar JSON och TOML, men YAML är ofta föredraget för dess läsbarhet. Implementationen i Go kräver noggrann användning av indentering och förståelse för hur man mappar data till Go-strukturer.

## Se även
- YAML-specifikation: https://yaml.org/spec/1.2/spec.html
- Go yaml.v2-dokumentation: https://pkg.go.dev/gopkg.in/yaml.v2
- Go yaml.v3-dokumentation: https://pkg.go.dev/gopkg.in/yaml.v3
- En jämförelse mellan konfigureringsformat: https://atc.wiki/job-aid/compare-config-formats
