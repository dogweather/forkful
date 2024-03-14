---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:30.966260-07:00
description: "Die Arbeit mit YAML in Go beinhaltet das Parsen von YAML-Dateien (YAML\
  \ Ain't Markup Language), einem menschenfreundlichen Daten-Serialisierungsstandard,\u2026"
lastmod: '2024-03-13T22:44:53.310092-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit YAML in Go beinhaltet das Parsen von YAML-Dateien (YAML Ain't\
  \ Markup Language), einem menschenfreundlichen Daten-Serialisierungsstandard,\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit YAML in Go beinhaltet das Parsen von YAML-Dateien (YAML Ain't Markup Language), einem menschenfreundlichen Daten-Serialisierungsstandard, in Go-Datenstrukturen und umgekehrt. Programmierer tun dies, um die Einfachheit und Lesbarkeit von YAML für Konfigurationsdateien, Anwendungseinstellungen oder den Datenaustausch zwischen Diensten und Komponenten, die in verschiedenen Sprachen geschrieben sind, zu nutzen.

## Wie geht das:

Um mit YAML in Go zu arbeiten, müssen Sie zunächst eine Bibliothek importieren, die das Parsen und Serialisieren von YAML unterstützt, da die Standardbibliothek von Go keine direkte Unterstützung für YAML beinhaltet. Die beliebteste Bibliothek für diesen Zweck ist "gopkg.in/yaml.v3". So fangen Sie an:

1. **Installieren des YAML-Pakets:**

```bash
go get gopkg.in/yaml.v3
```

2. **YAML in eine Go-Struktur parsen:**

Definieren Sie zunächst eine Struktur in Go, die der Struktur Ihrer YAML-Daten entspricht.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("User: %s\nPassword: %s\n", config.Database.User, config.Database.Password)
}
```

**Beispielausgabe:**

```
User: admin
Password: secret
```

3. **Eine Go-Struktur in YAML serialisieren:**

So konvertieren Sie eine Go-Struktur zurück in YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**Beispielausgabe:**

```yaml
---
database:
  user: admin
  password: supersecret
```

## Tiefergehende Betrachtung:

Die Verwendung von YAML in der Softwareentwicklung hat zugenommen, aufgrund seines menschenlesbaren Formats, was es zur idealen Wahl für Konfigurationsdateien, Dokumentation oder Datenaustauschformate macht. Im Vergleich zu JSON, seinem Pendant, bietet YAML Kommentare, skalare Typen und Beziehungsfunktionen, was einen reichhaltigeren Daten-Serialisierungsrahmen zur Verfügung stellt. Allerdings kommt die Flexibilität und Funktionsvielfalt mit einer erhöhten Komplexität beim Parsen und führt zu potenziellen Sicherheitsrisiken, wenn sie nicht sorgfältig gehandhabt werden (z.B. willkürliche Codeausführung).

Die Bibliothek "gopkg.in/yaml.v3" für Go ist eine robuste Lösung für die YAML-Verarbeitung, die einen Ausgleich zwischen Benutzerfreundlichkeit und umfassender Funktionsunterstützung schafft. Derzeit, während es Alternativen wie "go-yaml/yaml" gibt (die Bibliothek hinter "gopkg.in/yaml.v3"), hängt die gewählte Version meist von spezifischen Projektanforderungen oder persönlichen Vorlieben ab. Bei der Arbeit mit großen Datensätzen oder leistungskritischen Anwendungen könnten Programmierer einfachere Formate wie JSON in Betracht ziehen wegen ihrer reduzierten Parszeit und des geringeren Speicherbedarfs. Dennoch bleiben für Konfigurationsdateien oder Einstellungen, bei denen Lesbarkeit und Benutzerfreundlichkeit im Vordergrund stehen, YAML eine starke Option im Go-Ökosystem.
