---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:54.669511-07:00
description: "Die Arbeit mit JSON (JavaScript Object Notation) in Go umfasst das Kodieren\
  \ und Dekodieren von Daten zwischen Go-Datenstrukturen und dem JSON-Format.\u2026"
lastmod: '2024-03-13T22:44:53.311182-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit JSON (JavaScript Object Notation) in Go umfasst das Kodieren\
  \ und Dekodieren von Daten zwischen Go-Datenstrukturen und dem JSON-Format.\u2026"
title: Arbeiten mit JSON
weight: 38
---

## Was & Warum?

Die Arbeit mit JSON (JavaScript Object Notation) in Go umfasst das Kodieren und Dekodieren von Daten zwischen Go-Datenstrukturen und dem JSON-Format. Diese Aufgabe ist allgegenwärtig in Webdiensten und APIs, da JSON als ein leichtgewichtiges, textbasiertes und sprachunabhängiges Datenübertragungsformat dient, das eine einfache Datenfreigabe über verschiedene Programmierumgebungen hinweg ermöglicht.

## Wie:

In Go ist das Paket `encoding/json` Ihr Tor zur JSON-Manipulation und bietet Mechanismen, um Go-Datenstrukturen zu JSON (Marshalling) und zurück (Unmarshalling) zu konvertieren. Unten finden Sie grundlegende Beispiele, um zu beginnen:

### Kodierung (Marshalling)

Um eine Go-Struktur in JSON umzuwandeln, können Sie `json.Marshal` verwenden. Betrachten Sie die folgende Go-Struktur:

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

Ausgabe:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Dekodierung (Unmarshalling)

Um JSON in eine Go-Datenstruktur zu parsen, verwenden Sie `json.Unmarshal`:

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

Angesichts der Struktur `User` wie zuvor, wandelt dieser Code die JSON-Zeichenkette in eine User-Instanz um.

Ausgabe:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## Vertiefung

Das Paket `encoding/json` in Go bietet eine einfache API, die einen Großteil der Komplexität bei der JSON-Manipulation abstrahiert. Früh in der Entwicklung von Go eingeführt, spiegelt dieses Paket die Philosophie von Einfachheit und Effizienz in Go wider. Allerdings kann die Verwendung von Reflection durch `encoding/json`, um Strukturen zur Laufzeit zu inspizieren und zu modifizieren, zu einer weniger als optimalen Leistung in CPU-intensiven Szenarien führen.

Alternativen wie `json-iterator/go` und `ffjson` sind aufgetaucht und bieten eine schnellere JSON-Verarbeitung, indem statischer Marshalling- und Unmarshalling-Code erzeugt wird. Jedoch bleibt `encoding/json` das am häufigsten verwendete Paket aufgrund seiner Einfachheit, Robustheit und der Tatsache, dass es Teil der Standardbibliothek ist, was Kompatibilität und Stabilität über Go-Versionen hinweg gewährleistet.

Trotz seiner relativ langsameren Leistung macht die Benutzerfreundlichkeit und die Integration in das Go-Typsystem `encoding/json` für die meisten Anwendungen geeignet. Für diejenigen, die in Kontexten arbeiten, in denen Leistung von höchster Bedeutung ist, könnte die Erforschung externer Bibliotheken lohnenswert sein, aber für viele bietet die Standardbibliothek die richtige Balance zwischen Geschwindigkeit, Einfachheit und Zuverlässigkeit.
