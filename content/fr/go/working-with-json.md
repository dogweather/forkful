---
title:                "Manipulation de JSON"
date:                  2024-01-19
simple_title:         "Manipulation de JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
(Quoi et Pourquoi ?)
JSON, c'est le format d'échange de données. On l'utilise pour sa simplicité avec les APIs web et sa compatibilité avec de nombreux langages.

## How to:
(Comment faire :)
```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

// Définition d'une structure de données
type Personne struct {
    Nom    string `json:"nom"`
    Age    int    `json:"age"`
    Emails []string `json:"emails"`
}

func main() {
    // JSON exemple
    jsonData := `{"nom": "Dupont", "age": 25, "emails": ["dupont@example.com", "jd@example.fr"]}`
    
    // Decode JSON
    var p Personne
    err := json.Unmarshal([]byte(jsonData), &p)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Nom: %s\nAge: %d\nEmails: %v\n", p.Nom, p.Age, p.Emails)
    
    // Encode JSON
    nouveauJson, err := json.Marshal(p)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(nouveauJson))
}
```
Output:
```
Nom: Dupont
Age: 25
Emails: [dupont@example.com jd@example.fr]
{"nom":"Dupont","age":25,"emails":["dupont@example.com","jd@example.fr"]}
```

## Deep Dive:
(Plongée en profondeur)
JSON est apparu en 2001, conçu par Douglas Crockford. XML était la norme avant mais JSON l'a dépassé pour sa légèreté. Par rapport aux alternatives comme YAML, JSON maintient un bon équilibre de lisibilité et d'analyse aisée. En Go, `encoding/json` gère la sérialisation.

## See Also:
(Voir aussi)
- [Documentation JSON Go](https://golang.org/pkg/encoding/json/)
- [JSON sur Wikipédia](https://fr.wikipedia.org/wiki/JavaScript_Object_Notation)
- [Blog Go sur JSON](https://blog.golang.org/json)
