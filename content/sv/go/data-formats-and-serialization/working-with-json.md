---
aliases:
- /sv/go/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:11.366892-07:00
description: "Att arbeta med JSON (JavaScript Object Notation) i Go inneb\xE4r att\
  \ koda om och avkoda data mellan Go datastrukturer och JSON-format. Denna uppgift\
  \ \xE4r\u2026"
lastmod: 2024-02-18 23:08:51.355773
model: gpt-4-0125-preview
summary: "Att arbeta med JSON (JavaScript Object Notation) i Go inneb\xE4r att koda\
  \ om och avkoda data mellan Go datastrukturer och JSON-format. Denna uppgift \xE4\
  r\u2026"
title: Att Arbeta med JSON
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON (JavaScript Object Notation) i Go innebär att koda om och avkoda data mellan Go datastrukturer och JSON-format. Denna uppgift är allestädes närvarande inom webbtjänster och API:er, eftersom JSON fungerar som ett lättviktigt, textbaserat och språkoberoende datautbytesformat, vilket möjliggör enkel dataöverföring över olika programmeringsmiljöer.

## Hur man gör:

I Go är paketet `encoding/json` din ingång till manipulation av JSON, och tillhandahåller mekanismer för att konvertera Go-datastrukturer till JSON (marshalling) och tillbaka (unmarshalling). Nedan följer grundläggande exempel för att komma igång:

### Kodning (Marshalling)

För att konvertera en Go struct till JSON, kan du använda `json.Marshal`. Betrakta följande Go struct:

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

Utskrift:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Avkodning (Unmarshalling)

För att tolka JSON till en Go datastruktur, använd `json.Unmarshal`:

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

Givet struct `User` som tidigare, tolkar denna kod JSON-strängen till en User-instans.

Utskrift:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## Djupdykning

Paketet `encoding/json` i Go erbjuder ett rakt på sak API som abstraherar mycket av komplexiteten involverad i JSON-manipulation. Introducerat tidigt i Gos utveckling, reflekterar detta paket Gofilosofi av enkelhet och effektivitet. Dock kan användningen av reflektion av `encoding/json` för att inspektera och modifiera strukturer vid körning leda till prestanda som är mindre än optimal i CPU-intensiva scenarier.

Alternativ som `json-iterator/go` och `ffjson` har dykt upp och erbjuder snabbare JSON-behandling genom att generera statisk kod för marshalling och unmarshalling. Dock är `encoding/json` fortfarande det mest använda paketet på grund av sin enkelhet, robusthet och det faktum att det ingår i standardbiblioteket, vilket säkerställer kompatibilitet och stabilitet över Go-versioner.

Trots sin långsammare relativa prestanda, gör användarvänligheten och integrationen med Gos typsystem att `encoding/json` passar för de flesta applikationer. För de som arbetar i sammanhang där prestanda är av högsta vikt, kan det vara värt att utforska externa bibliotek, men för många lyckas standardbiblioteket uppnå en bra balans mellan hastighet, enkelhet och pålitlighet.
