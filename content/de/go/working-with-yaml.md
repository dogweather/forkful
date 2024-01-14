---
title:                "Go: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit YAML beschäftigen? Nun, YAML ist eine einfache, menschenlesbare Datenformatierungssprache, die im Gegensatz zu anderen komplexen Formaten wie JSON oder XML weniger fehleranfällig ist. Es ist auch sehr flexibel und kann verwendet werden, um Daten in einer Vielzahl von Anwendungen zu strukturieren.

## Wie man es macht

Um mit YAML in Go zu arbeiten, müssen wir zuerst das entsprechende Paket importieren:

```
import "gopkg.in/yaml.v2"
```

Dann können wir unsere YAML-Datei einlesen und dekodieren:

```
// YAML-Datei einlesen
yamlFile, err := ioutil.ReadFile("beispiel.yaml")

// Fehlerbehandlung
if err != nil {
    log.Fatalf("Fehler beim Einlesen der YAML-Datei: %v", err)
}

// YAML-Datei dekodieren
var daten map[string]interface{}

err = yaml.Unmarshal(yamlFile, &daten)

// Fehlerbehandlung
if err != nil {
    log.Fatalf("Fehler beim Dekodieren der YAML-Datei: %v", err)
}
```

Nun können wir auf die Daten zugreifen, indem wir den entsprechenden Schlüssel angeben:

```
// Daten auslesen
daten["name"] // Ausgabe: "Max Mustermann"
daten["alter"] // Ausgabe: 32
```

## Tiefergehende Informationen

Eine interessante Funktion von YAML ist, dass es auch Variablen unterstützt. Wir können also zum Beispiel einen Wert in einer Variable definieren und sie dann in unsere YAML-Datei einbinden:

```
// Variable definieren
name := "Max Mustermann"

// YAML-Datei mit Variablen
person:
  name: {{name}}
  alter: 32
```

YAML unterstützt auch die Verwendung von Listen und verschachtelten Datenstrukturen, um komplexe Informationen übersichtlich darzustellen. Es gibt noch viele weitere Möglichkeiten und Funktionen von YAML, die es zu entdecken gilt.

## Siehe auch

- [Offizielle Go YAML Paket Dokumentation](https://pkg.go.dev/gopkg.in/yaml.v2)
- [Interaktiver YAML Tutorial](https://yaml-multiline.info/)
- [Einführung in YAML auf DEVMEDIUM](https://medium.com/devmedium/introduction-to-yaml-ed30c1d87992)