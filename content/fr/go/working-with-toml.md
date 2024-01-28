---
title:                "Travailler avec TOML"
date:                  2024-01-26T04:22:04.768392-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-toml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec TOML consiste à analyser et encoder des fichiers TOML (Tom's Obvious, Minimal Language) en Go. Les programmeurs optent pour TOML pour sa lisibilité et sa cartographie facile aux structures de données, ce qui le rend parfait pour les configurations.

## Comment faire :
Pour travailler avec TOML en Go, vous utiliserez généralement une bibliothèque telle que `BurntSushi/toml`. Voici un aperçu rapide de l'analyse d'un fichier de configuration TOML :

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
    fmt.Printf("Titre : %s, Propriétaire : %s\n", config.Title, config.Owner.Name)
}
```

Exemple de `config.toml` :

```Toml
title = "Exemple TOML"
[owner]
name = "Tom Preston-Werner"
```

Exemple de sortie :

```
Titre : Exemple TOML, Propriétaire : Tom Preston-Werner
```

## Exploration Approfondie
TOML, introduit par Tom Preston-Werner en 2013, a été conçu pour être un format de fichier de configuration minimal facile à lire grâce à sa sémantique claire. Les développeurs Go utilisent souvent TOML pour les configurations plutôt que des alternatives comme JSON ou YAML pour sa simplicité et sa capacité à représenter des hiérarchies complexes simplement.

Compared to YAML, which has complex features and potential security concerns, TOML's flat design reduces complexity and typo-induced errors. Par rapport à YAML, qui possède des fonctionnalités complexes et des préoccupations potentielles de sécurité, la conception plate de TOML réduit la complexité et les erreurs induites par les fautes de frappe. Et contrairement à JSON, TOML prend en charge les commentaires, ce qui facilite l'explication des configurations en ligne.

Lorsque vous travaillez avec TOML en Go, il y a des nuances à considérer. Les balises de structure peuvent personnaliser la manière dont vos structures sont mappées aux structures TOML, et vous devez également être conscient de la manière dont les tableaux TOML et les tables en ligne sont analysés en tranches et cartes Go.

## Voir Aussi
- Spécification TOML : https://toml.io/fr/
- Bibliothèque BurntSushi/toml : https://github.com/BurntSushi/toml
- Une comparaison des formats de fichiers de configuration : https://www.redhat.com/sysadmin/yaml-toml-json-differences
