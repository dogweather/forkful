---
title:                "Arbeiten mit TOML"
date:                  2024-01-26T04:22:16.293011-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit TOML beinhaltet das Parsen und Kodieren von TOML (Tom's Offensichtliche, Minimale Sprache)-Dateien in Go. Programmierer entscheiden sich für TOML aufgrund seiner Lesbarkeit und einfachen Zuordnung zu Datenstrukturen, was es zu einer soliden Wahl für Konfigurationen macht.

## Wie:
Um mit TOML in Go zu arbeiten, verwendet man in der Regel eine Bibliothek wie `BurntSushi/toml`. Hier ist ein kurzer Blick darauf, wie man eine TOML-Konfigurationsdatei parsed:

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
    fmt.Printf("Titel: %s, Eigentümer: %s\n", config.Title, config.Owner.Name)
}
```

Beispiel `config.toml`:

```Toml
title = "Beispiel TOML"
[owner]
name = "Tom Preston-Werner"
```

Beispielausgabe:

```
Titel: Beispiel TOML, Eigentümer: Tom Preston-Werner
```

## Tiefere Betrachtung
TOML, eingeführt von Tom Preston-Werner im Jahr 2013, wurde als minimales Konfigurationsdateiformat entworfen, das aufgrund seiner klaren Semantik leicht zu lesen ist. Go-Entwickler verwenden oft TOML für Konfigurationen anstelle von Alternativen wie JSON oder YAML, wegen seiner Direktheit und Fähigkeit, komplexe Hierarchien einfach darzustellen.

Im Vergleich zu YAML, das komplexe Funktionen und potentielle Sicherheitsbedenken hat, reduziert das flache Design von TOML Komplexität und Tippfehler-induzierte Fehler. Und anders als JSON unterstützt TOML Kommentare, was es einfacher macht, Konfigurationen in der Zeile zu erklären.

Wenn man mit TOML in Go arbeitet, gibt es Nuancen zu beachten. Struktur-Tags können anpassen, wie Ihre Strukturen auf TOML-Strukturen abgebildet werden, und man sollte auch wissen, wie TOML-Arrays und Inline-Tabellen in Go-Slices und -Maps geparst werden.

## Siehe Auch
- TOML Spezifikation: https://toml.io/en/
- BurntSushi/toml Bibliothek: https://github.com/BurntSushi/toml
- Ein Vergleich von Konfigurationsdateiformaten: https://www.redhat.com/sysadmin/yaml-toml-json-differences