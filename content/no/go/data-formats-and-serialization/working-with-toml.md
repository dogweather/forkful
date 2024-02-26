---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:37.510676-07:00
description: "TOML (Tom's Obvious, Minimal Language) er et konfigurasjonsfilformat\
  \ som er enkelt \xE5 lese p\xE5 grunn av sin enkle syntaks. Programmerere bruker\
  \ TOML for \xE5\u2026"
lastmod: '2024-02-25T18:49:38.523964-07:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) er et konfigurasjonsfilformat som\
  \ er enkelt \xE5 lese p\xE5 grunn av sin enkle syntaks. Programmerere bruker TOML\
  \ for \xE5\u2026"
title: "\xC5 jobbe med TOML"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

TOML (Tom's Obvious, Minimal Language) er et konfigurasjonsfilformat som er enkelt å lese på grunn av sin enkle syntaks. Programmerere bruker TOML for å konfigurere applikasjonsinnstillinger og avhengigheter på grunn av dets klarhet og rett frem kartlegging til datastrukturer, noe som gjør det til et populært valg i mange Go-prosjekter for å sette opp og håndtere konfigurasjoner.

## Hvordan:

For å begynne å jobbe med TOML i Go, må du først inkludere et bibliotek som kan parse TOML-filer, siden Go-standardbiblioteket ikke støtter TOML nativt. Pakken `BurntSushi/toml` er et populært valg for dette. Først, sørg for å installere det:

```bash
go get github.com/BurntSushi/toml
```

Her er et enkelt eksempel på hvordan du bruker det. Tenk deg at du har en konfigurasjonsfil med navnet `config.toml` med følgende innhold:

```toml
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Nå trenger du å lage en Go-struktur som speiler TOML-strukturen:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Tittel: %s\n", config.Title)
    fmt.Printf("Databaseserver: %s\n", config.Database.Server)
}
```

Eksempelutdata:

```
Tittel: TOML Example
Databaseserver: 192.168.1.1
```

## Dypdykk

TOML ble opprettet av Tom Preston-Werner, en av medgrunnleggerne av GitHub, for å tilby et enkelt konfigurasjonsfilformat som enkelt kan kartlegges til en hashtabell og forstås ved første øyekast uten forkunnskaper om formatet. Det står i kontrast til JSON eller YAML, som, selv om de også er mye brukt, kan være mindre menneskevennlige for konfigurasjonsfiler på grunn av parenteser, anførselstegn og innrykksproblemer.

Pakken `BurntSushi/toml` i Go er et robust bibliotek som ikke bare tillater dekoding, men også koding av TOML-filer, noe som gjør det til et allsidig valg for applikasjoner som trenger å både lese og skrive konfigurasjonsfiler i dette formatet. Imidlertid bør man merke seg at med fremskritt innen teknologier og innføringen av nyere Go-versjoner, har alternativer som `pelletier/go-toml` dukket opp, som tilbyr forbedret ytelse og ekstra funksjoner som tremanipulering og støtte for spørringer.

Selv om TOML er et flott valg for mange applikasjoner, avhengig av applikasjonens konfigurasjonskompleksitet og personlige eller lagpreferanser, kan andre formater som YAML eller JSON være bedre egnet, spesielt hvis konfigurasjonen krever mer komplekse datastrukturer som TOMLs ordrike natur kanskje ikke elegant kan fange. Likevel, for enkle, lesbare og lett redigerbare konfigurasjoner, er TOML, sammen med Gos sterke typesystem og de nevnte bibliotekene, et utmerket valg.
