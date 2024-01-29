---
title:                "Werken met TOML"
date:                  2024-01-28T22:11:07.751380-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met TOML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met TOML omvat het parseren en coderen van TOML (Tom's Obvious, Minimal Language) bestanden in Go. Programmeurs kiezen voor TOML vanwege de leesbaarheid en eenvoudige koppeling aan datastructuren, een solide keuze voor configuraties.

## Hoe te:
Om met TOML in Go te werken, gebruik je doorgaans een bibliotheek zoals `BurntSushi/toml`. Hier is een snelle blik op het parseren van een TOML-configuratiebestand:

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

Voorbeeld `config.toml`:

```Toml
title = "Voorbeeld TOML"
[owner]
name = "Tom Preston-Werner"
```

Voorbeelduitvoer:

```
Title: Voorbeeld TOML, Owner: Tom Preston-Werner
```

## Diepgaande verkenning
TOML, geïntroduceerd door Tom Preston-Werner in 2013, is ontworpen als een minimale configuratiebestandsindeling die gemakkelijk te lezen is vanwege de duidelijke semantiek. Go-ontwikkelaars gebruiken vaak TOML voor configuratie in plaats van alternatieven zoals JSON of YAML vanwege de eenvoud en het vermogen om complexe hiërarchieën simpel te representeren.

Vergeleken met YAML, dat complexe functies heeft en potentiële beveiligingsproblemen, reduceert het vlakke ontwerp van TOML de complexiteit en door typfouten veroorzaakte fouten. En in tegenstelling tot JSON, ondersteunt TOML commentaren, wat het gemakkelijker maakt om configuraties in-line uit te leggen.

Bij het werken met TOML in Go zijn er nuances om te overwegen. Struct-tags kunnen aanpassen hoe uw structuren koppelen aan TOML-structuren, en u moet ook bewust zijn van hoe TOML-arrays en inline tabellen worden geparseerd naar Go-slices en maps.

## Zie ook
- TOML-specificatie: https://toml.io/en/
- BurntSushi/toml Bibliotheek: https://github.com/BurntSushi/toml
- Een vergelijking van configuratiebestandsformaten: https://www.redhat.com/sysadmin/yaml-toml-json-differences
