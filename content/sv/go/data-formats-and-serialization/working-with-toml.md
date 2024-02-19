---
aliases:
- /sv/go/working-with-toml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:42.225769-07:00
description: "TOML (Toms Uppenbara, Minimala Spr\xE5k) \xE4r ett filformat f\xF6r\
  \ konfiguration som \xE4r l\xE4tt att l\xE4sa tack vare sin enkla syntax. Programmerare\
  \ anv\xE4nder TOML f\xF6r\u2026"
lastmod: 2024-02-18 23:08:51.357852
model: gpt-4-0125-preview
summary: "TOML (Toms Uppenbara, Minimala Spr\xE5k) \xE4r ett filformat f\xF6r konfiguration\
  \ som \xE4r l\xE4tt att l\xE4sa tack vare sin enkla syntax. Programmerare anv\xE4\
  nder TOML f\xF6r\u2026"
title: Att Arbeta med TOML
---

{{< edit_this_page >}}

## Vad & Varför?

TOML (Toms Uppenbara, Minimala Språk) är ett filformat för konfiguration som är lätt att läsa tack vare sin enkla syntax. Programmerare använder TOML för att konfigurera applikationsinställningar och beroenden på grund av dess tydlighet och enkla mappning till datastrukturer, vilket gör det till ett populärt val i många Go-projekt för att sätta upp och hantera konfigurationer.

## Hur man gör:

För att börja arbeta med TOML i Go måste du först inkludera ett bibliotek som kan tolka TOML-filer eftersom Golangs standardbibliotek inte stöder TOML nativt. Paketet `BurntSushi/toml` är ett populärt val för detta. Se först till att installera det:

```bash
go get github.com/BurntSushi/toml
```

Här är ett enkelt exempel på hur man använder det. Anta att du har en konfigurationsfil med namnet `config.toml` med följande innehåll:

```toml
title = "Exempel på TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Nu måste du skapa en Go-struktur som speglar TOML-strukturen:

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
    fmt.Printf("Databasserver: %s\n", config.Database.Server)
}
```

Exempelutdata:

```
Titel: Exempel på TOML
Databasserver: 192.168.1.1
```

## Fördjupning

TOML skapades av Tom Preston-Werner, en av medgrundarna till GitHub, för att erbjuda ett rakt på sak filformat för konfiguration som enkelt kan mappas till en hashtabell och förstås med en ögonkastning utan förkunskaper om formatet. Det står i kontrast till JSON eller YAML som, även om de också är brett använda, kan vara mindre användarvänliga för konfigurationsfiler på grund av klammerparenteser, citattecken och indenteringsproblem.

Paketet `BurntSushi/toml` i Go är ett robust bibliotek som inte bara tillåter avkodning utan också kodning av TOML-filer, vilket gör det till ett mångsidigt val för applikationer som behöver läsa och skriva konfigurationsfiler i detta format. Man bör dock notera att med framsteg i tekniker och introduktionen av nya Go-versioner har alternativ som `pelletier/go-toml` framträtt, som erbjuder förbättrad prestanda och ytterligare funktioner som trädsmanipulation och frågestöd.

Medan TOML är ett utmärkt val för många applikationer, beroende på applikationskonfigurationens komplexitet och personliga eller teampreferenser, kan andra format som YAML eller JSON vara mer lämpliga, särskilt om konfigurationen kräver mer komplexa datastrukturer som TOML:s ordrika natur kanske inte elegant fångar. Icke desto mindre, för enkel, läsbar och lättredigerbar konfiguration är TOML, tillsammans med Gos starka typsystem och de nämnda biblioteken, ett utmärkt val.
