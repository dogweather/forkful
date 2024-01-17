---
title:                "Travailler avec yaml"
html_title:           "Go: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-yaml.md"
---

{{< edit_this_page >}}

---

# Utiliser YAML avec Go

## Qu'est-ce que c'est et pourquoi les programmeurs l'utilisent?

Travailler avec YAML, ou "YAML Ain't Markup Language", est une façon de stocker et de transférer des données dans un format lisible pour les humains. Les programmeurs utilisent YAML car il est facile à comprendre et à modifier, et il peut être utilisé pour configurer des applications ou des scripts.

## Comment faire:

Voici un exemple simple de YAML en Go:

```go
import (
    "fmt"

    "github.com/go-yaml/yaml"
)

func main() {
    data := `
        name: John
        age: 25
        occupation: Developer`

    var m map[string]interface{}
    err := yaml.Unmarshal([]byte(data), &m)
    if err != nil {
        panic(err)
    }
    fmt.Println(m["name"]) // Output: John
}
```

## Plongée en profondeur:

- Contexte historique: YAML a été développé en 2001 pour résoudre les problèmes de formats de données complexes et non lisibles.
- Alternatives: JSON est un autre format populaire pour stocker et transférer des données, mais YAML offre une syntaxe plus lisible pour les humains.
- Détails d'implémentation: Go a une bibliothèque standard pour travailler avec YAML, mais il existe également des bibliothèques tierces telles que "github.com/go-yaml/yaml".

## À voir également:

- Documentation officielle de YAML: https://yaml.org/
- Documentation officielle de Go pour le package YAML: https://pkg.go.dev/gopkg.in/yaml.v3