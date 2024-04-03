---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:37.350037-07:00
description: "Wie geht das: Um mit TOML in Go zu beginnen, m\xFCssen Sie zun\xE4chst\
  \ eine Bibliothek einbinden, die TOML-Dateien parsen kann, da die Standardbibliothek\
  \ von Go\u2026"
lastmod: '2024-03-13T22:44:53.313279-06:00'
model: gpt-4-0125-preview
summary: "Um mit TOML in Go zu beginnen, m\xFCssen Sie zun\xE4chst eine Bibliothek\
  \ einbinden, die TOML-Dateien parsen kann, da die Standardbibliothek von Go TOML\
  \ nicht nativ unterst\xFCtzt."
title: Arbeiten mit TOML
weight: 39
---

## Wie geht das:
Um mit TOML in Go zu beginnen, müssen Sie zunächst eine Bibliothek einbinden, die TOML-Dateien parsen kann, da die Standardbibliothek von Go TOML nicht nativ unterstützt. Das Paket `BurntSushi/toml` ist hierfür eine beliebte Wahl. Stellen Sie zuerst sicher, dass es installiert ist:

```bash
go get github.com/BurntSushi/toml
```

Hier ist ein einfaches Beispiel, wie man es verwendet. Angenommen, Sie haben eine Konfigurationsdatei namens `config.toml` mit dem folgenden Inhalt:

```toml
title = "TOML Beispiel"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Nun müssen Sie eine Go-Struktur erstellen, die der TOML-Struktur entspricht:

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
    fmt.Printf("Datenbankserver: %s\n", config.Database.Server)
}
```

Beispielausgabe:

```
Titel: TOML Beispiel
Datenbankserver: 192.168.1.1
```

## Tiefergehend
TOML wurde von Tom Preston-Werner, einem der Mitbegründer von GitHub, kreiert, um ein simples Konfigurationsdateiformat anzubieten, das leicht auf eine Hashtabelle abgebildet werden kann und auf einen Blick verstanden wird, ohne dass Vorwissen über das Format erforderlich ist. Es steht im Kontrast zu JSON oder YAML, die, obwohl auch weit verbreitet, für Konfigurationsdateien wegen Klammern, Anführungszeichen und Einrückungsproblemen weniger benutzerfreundlich sein können.

Das Paket `BurntSushi/toml` in Go ist eine robuste Bibliothek, die nicht nur das Dekodieren, sondern auch das Kodieren von TOML-Dateien ermöglicht, was es zu einer vielseitigen Wahl für Anwendungen macht, die Konfigurationsdateien in diesem Format sowohl lesen als auch schreiben müssen. Allerdings sollte man beachten, dass mit dem Fortschritt der Technologien und der Einführung neuerer Go-Versionen Alternativen wie `pelletier/go-toml` aufgetaucht sind, die eine verbesserte Leistung und zusätzliche Funktionen wie Baummanipulation und Unterstützung für Abfragen bieten.

Obwohl TOML für viele Anwendungen eine großartige Wahl ist, können je nach Komplexität der Anwendungskonfiguration und persönlichen oder Teampräferenzen andere Formate wie YAML oder JSON besser geeignet sein, besonders wenn die Konfiguration komplexere Datenstrukturen erfordert, die die wortreiche Art von TOML möglicherweise nicht elegant einfangen kann. Dennoch ist für unkomplizierte, lesbare und leicht editierbare Konfigurationen TOML in Kombination mit Go’s starkem Typsystem und den genannten Bibliotheken eine ausgezeichnete Wahl.
