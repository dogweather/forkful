---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:00.926256-07:00
description: "Werken met JSON (JavaScript Object Notation) in Go houdt in dat er gegevens\
  \ worden gecodeerd en gedecodeerd tussen Go datastructuren en het JSON-formaat.\u2026"
lastmod: 2024-02-19 22:05:09.394972
model: gpt-4-0125-preview
summary: "Werken met JSON (JavaScript Object Notation) in Go houdt in dat er gegevens\
  \ worden gecodeerd en gedecodeerd tussen Go datastructuren en het JSON-formaat.\u2026"
title: Werken met JSON
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met JSON (JavaScript Object Notation) in Go houdt in dat er gegevens worden gecodeerd en gedecodeerd tussen Go datastructuren en het JSON-formaat. Deze taak is alomtegenwoordig in webdiensten en API's, aangezien JSON fungeert als een lichtgewicht, op tekst gebaseerd en taalonafhankelijk gegevensuitwisselingsformaat, dat eenvoudige gegevensdeling mogelijk maakt over verschillende programmeeromgevingen.

## Hoe:

In Go is het `encoding/json` pakket jouw toegangspoort tot JSON-manipulatie, en biedt mechanismen om Go datastructuren naar JSON (marshalen) en terug (unmarshalen) te converteren. Hieronder volgen enkele basisvoorbeelden om je op weg te helpen:

### Encoderen (Marshalen)

Om een Go struct naar JSON te converteren, kun je `json.Marshal` gebruiken. Beschouw de volgende Go struct:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

Uitvoer:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Decoderen (Unmarshalen)

Om JSON in een Go datastructuur te parseren, gebruik je `json.Unmarshal`:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

Gegeven de struct `User` als voorheen, parseert deze code de JSON-string naar een User-instance.

Uitvoer:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## Diepere Duik

Het `encoding/json` pakket in Go biedt een eenvoudige API die veel van de complexiteit van JSON-manipulatie abstraheert. Geïntroduceerd in de vroege ontwikkeling van Go, weerspiegelt dit pakket de filosofie van Go op het gebied van eenvoud en efficiëntie. Echter, het gebruik van reflectie door `encoding/json` om structs in runtime te inspecteren en te wijzigen, kan leiden tot prestaties die minder dan optimaal zijn in CPU-intensieve scenario's.

Alternatieven zoals `json-iterator/go` en `ffjson` zijn ontstaan, welke snellere JSON-verwerking bieden door het genereren van statische marshal- en unmarshal-code. Desondanks blijft `encoding/json` het meest gebruikte pakket vanwege zijn eenvoud, robuustheid en het feit dat het deel uitmaakt van de standaardbibliotheek, wat compatibiliteit en stabiliteit over Go-versies heen verzekert.

Ondanks de relatief tragere prestaties, maakt het gebruiksgemak en de integratie met het type systeem van Go `encoding/json` geschikt voor de meeste toepassingen. Voor degenen die werken in contexten waar prestaties van het grootste belang zijn, kan het verkennen van externe bibliotheken de moeite waard zijn, maar voor velen biedt de standaardbibliotheek de juiste balans tussen snelheid, eenvoud en betrouwbaarheid.
