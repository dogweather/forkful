---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Praca z YAML wiąże się z manipulowaniem danymi w lekkostrawnym dla człowieka formacie. Programiści używają YAML do konfiguracji, serializacji danych i komunikacji między różnymi elementami systemu.

## How to: (Jak to zrobić?)
Najpierw zainstalujmy pakiet `go-yaml` za pomocą `go get`:

```Go
go get gopkg.in/yaml.v2
```

Przykład kodu Go pokazujący jak odczytać YAML:

```Go
package main

import (
    "fmt"
    "log"
    "gopkg.in/yaml.v2"
)

type Config struct {
    Hostname string `yaml:"hostname"`
    Port     int    `yaml:"port"`
}

func main() {
    data := []byte(`
hostname: example.com
port: 1234
`)

    var config Config
    err := yaml.Unmarshal(data, &config)
    if err != nil {
        log.Fatalf("error: %v", err)
    }
    fmt.Printf("Hostname: %s, Port: %d\n", config.Hostname, config.Port)
}
```

Przykładowe wyniki:

```Go
Hostname: example.com, Port: 1234
```

## Deep Dive (W głąb tematu)
YAML, czyli YAML Ain't Markup Language, pojawił się w 2001 roku. Jest alternatywą dla XML i JSON, często używaną ze względu na czytelność i prostszą syntaktykę. Podczas pracy z YAML, ważne jest zrozumienie, że tabulatory nie są dozwolone; należy używać spacji. Go korzysta z pakietu `yaml` do parsowania i generowania YAML.

## See Also (Zobacz także)
- Oficjalna strona YAML: https://yaml.org
- Repozytorium `go-yaml`: https://github.com/go-yaml/yaml
- YAML w Go Tutorial: https://www.sohamkamani.com/blog/golang/2018-07-19-golang-omitempty/
