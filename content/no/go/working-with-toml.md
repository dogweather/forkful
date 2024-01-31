---
title:                "Jobbe med TOML"
date:                  2024-01-26T04:22:16.027383-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med TOML involverer parsing og koding av TOML (Tom's Obvious, Minimal Language) filer i Go. Programmerere velger TOML for dets lesbarhet og lette kartlegging til datastrukturer, en solid passform for konfigurasjoner.

## Hvordan:
For å jobbe med TOML i Go, vil du typisk bruke et bibliotek som `BurntSushi/toml`. Her er en rask titt på parsing av en TOML konfigurasjonsfil:

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

Eksempel `config.toml`:

```Toml
title = "Example TOML"
[owner]
name = "Tom Preston-Werner"
```

Eksempel på utdata:

```
Title: Example TOML, Owner: Tom Preston-Werner
```

## Dypdykk
TOML, introdusert av Tom Preston-Werner i 2013, ble designet for å være et minimalt konfigurasjonsfilformat som er lett å lese på grunn av dets klare semantikk. Go-utviklere bruker ofte TOML for konfigurasjon over alternativer som JSON eller YAML for dets direkthet og evne til å representere komplekse hierarkier med enkelhet.

Sammenlignet med YAML, som har komplekse funksjoner og potensielle sikkerhetsproblemer, reduserer TOMLs flate design kompleksitet og feil forårsaket av skrivefeil. Og i motsetning til JSON, støtter TOML kommentarer, noe som gjør det enklere å forklare konfigurasjoner på linje.

Når du jobber med TOML i Go, er det nyanser å vurdere. Struktur-tags kan tilpasse hvordan dine strukturer kartlegges til TOML-strukturer, og du bør også være oppmerksom på hvordan TOML-arrays og innebygde tabeller parses til Go-slices og maps.

## Se også
- TOML-spesifikasjon: https://toml.io/en/
- BurntSushi/toml Bibliotek: https://github.com/BurntSushi/toml
- En sammenligning av konfigurasjonsfilformater: https://www.redhat.com/sysadmin/yaml-toml-json-differences
