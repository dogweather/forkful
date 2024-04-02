---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:43.308560-07:00
description: "TOML (Tom's Obvious, Minimal Language) is een configuratiebestandsformaat\
  \ dat gemakkelijk te lezen is vanwege zijn eenvoudige syntaxis. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.311288-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) is een configuratiebestandsformaat\
  \ dat gemakkelijk te lezen is vanwege zijn eenvoudige syntaxis. Programmeurs\u2026"
title: Werken met TOML
weight: 39
---

## Wat & Waarom?

TOML (Tom's Obvious, Minimal Language) is een configuratiebestandsformaat dat gemakkelijk te lezen is vanwege zijn eenvoudige syntaxis. Programmeurs gebruiken TOML om applicatie-instellingen en afhankelijkheden te configureren vanwege de duidelijkheid en de directe toewijzing aan gegevensstructuren. Dit maakt het een populaire keuze in veel Go-projecten voor het instellen en beheren van configuraties.

## Hoe:

Om te beginnen met het werken met TOML in Go, moet je eerst een bibliotheek toevoegen die TOML-bestanden kan parseren, aangezien de standaardbibliotheek van Go geen native ondersteuning voor TOML biedt. Het `BurntSushi/toml` pakket is een populaire keuze hiervoor. Zorg eerst dat je het installeert:

```bash
go get github.com/BurntSushi/toml
```

Hier is een eenvoudig voorbeeld van hoe het te gebruiken. Stel je voor dat je een configuratiebestand genaamd `config.toml` hebt met de volgende inhoud:

```toml
title = "Voorbeeld van TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Nu moet je een Go-structuur maken die de TOML-structuur spiegelt:

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
    fmt.Printf("Titel: %s\n", config.Title)
    fmt.Printf("Database Server: %s\n", config.Database.Server)
}
```

Voorbeelduitvoer:

```
Titel: Voorbeeld van TOML
Database Server: 192.168.1.1
```

## Diepgaande duik

TOML is gecreëerd door Tom Preston-Werner, een van de medeoprichters van GitHub, om een eenvoudig leesbaar configuratiebestandsformaat te bieden dat gemakkelijk in kaart kan worden gebracht naar een hash-tabel en op het eerste gezicht begrepen kan worden zonder voorafgaande kennis van het formaat. Het contrasteert met JSON of YAML, die, hoewel ook veel gebruikt, minder mensvriendelijk kunnen zijn voor configuratiebestanden vanwege haakjes, aanhalingstekens en inspringingsproblemen.

Het `BurntSushi/toml` pakket in Go is een robuuste bibliotheek die niet alleen het decoderen, maar ook het coderen van TOML-bestanden mogelijk maakt, waardoor het een veelzijdige keuze is voor applicaties die configuratiebestanden in dit formaat moeten lezen en schrijven. Men moet echter opmerken dat met de vooruitgang van technologieën en de introductie van nieuwere Go-versies, alternatieven zoals `pelletier/go-toml` zijn ontstaan, die verbeterde prestaties en extra functies bieden, zoals boommanipulatie en ondersteuning van query’s.

Hoewel TOML voor veel toepassingen een geweldige keuze is, kunnen andere formaten zoals YAML of JSON, afhankelijk van de complexiteit van de applicatieconfiguratie en persoonlijke of teamvoorkeuren, beter geschikt zijn, vooral als de configuratie complexere gegevensstructuren vereist die de omslachtige aard van TOML mogelijk niet elegant vastlegt. Niettemin, voor eenvoudig leesbare, bewerkbare en begrijpelijke configuraties, is TOML, gecombineerd met het sterke typesysteem van Go en de bovengenoemde bibliotheken, een uitstekende keuze.
