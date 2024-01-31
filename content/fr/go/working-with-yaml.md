---
title:                "Travailler avec YAML"
date:                  2024-01-19
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec YAML en Go est tout sur la manipulation des données structurées: lire, écrire et modifier. Les programmeurs le font pour configurer des applis et échanger des données facilement.

## How to:
Installe le package `go-yaml/yaml` avec :

```bash
go get gopkg.in/yaml.v2
```

Définis une structure et lis le YAML :

```Go
package main

import (
    "fmt"
    "log"
    "gopkg.in/yaml.v2"
)

type Config struct {
    Version string
    IsEnabled bool
    Features []string
}

func main() {
    data := `
version: "3.0"
isEnabled: true
features: ["feature1", "feature2"]
`

    var config Config
    err := yaml.Unmarshal([]byte(data), &config)
    if err != nil {
        log.Fatalf("error: %v", err)
    }

    fmt.Printf("Version: %s\nIsEnabled: %t\nFeatures: %v\n",
               config.Version, config.IsEnabled, config.Features)
}
```

Si t'exécutes, tu verras :

```
Version: 3.0
IsEnabled: true
Features: [feature1 feature2]
```

## Deep Dive:
YAML date des années 2000, un compromis entre XML verbeux et JSON simple. Alternatives: `json` et `toml`. YAML gère bien les données complexes et configurations lues par des humains. Les détails d'implémentation comprennent la gestion des types, l'indentation stricte et les références croisées.

## See Also:
- Documentation officielle de `yaml` : https://pkg.go.dev/gopkg.in/yaml.v2
- YAML spécifications : https://yaml.org/spec/1.2/spec.html
- Comparaisons avec JSON et TOML : https://bitfieldconsulting.com/golang/serialize
