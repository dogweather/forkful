---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist ein menschenlesbares Datenformat für Konfigurationsdateien und Datenaustausch. Programmierer nutzen YAML wegen der Einfachheit und Klarheit, besonders bei Konfigurationsmanagement und Anwendungen, die komplexe Datenstrukturen benötigen.

## How to:
Um YAML in Go zu nutzen, verwenden wir das Paket `gopkg.in/yaml.v2` oder `gopkg.in/yaml.v3` für die neueste Version. Hier ein Beispiel zum Einlesen einer YAML-Konfigurationsdatei:

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v3"
	"io/ioutil"
	"log"
)

type Config struct {
	Version string
	Settings map[string]string
}

func main() {
	data, err := ioutil.ReadFile("config.yaml")
	if err != nil {
		log.Fatal(err)
	}

	var config Config

	err = yaml.Unmarshal(data, &config)
	if err != nil {
		log.Fatalf("error: %v", err)
	}

	fmt.Printf("Version: %s\nSettings: %v\n", config.Version, config.Settings)
}
```

Ausgabe (angenommen `config.yaml` enthält entsprechende Werte):
```
Version: 1.0
Settings: map[environment:production loglevel:debug]
```

## Deep Dive:
YAML entstand Anfang der 2000er als ein benutzerfreundlicheres Format im Vergleich zu XML. Im Go-Kontext konkurriert es mit anderen Formaten wie JSON, das in der Standardbibliothek eingebettet ist, aber YAML wird für seine Lesbarkeit und Kommentierungsmöglichkeiten bevorzugt. Die Go-Implementierung nutzt ein Marshall-Unmarshall-Muster, ähnlich wie bei JSON, um Go-Strukturen in YAML umzuwandeln und umgekehrt.

## See Also:
Weitere Informationen und Dokumentation bietet folgende Quellen:

- YAML v3 Paketdokumentation: https://pkg.go.dev/gopkg.in/yaml.v3
- Offizielle YAML-Website mit Spezifikationen: https://yaml.org
- Go-Standardbibliothek JSON-Handbuch (zum Vergleich): https://golang.org/pkg/encoding/json/
