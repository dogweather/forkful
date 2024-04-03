---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:54.862603-07:00
description: "Comment faire : En Go, le package `encoding/json` est votre porte d'entr\xE9\
  e pour la manipulation du JSON, offrant des m\xE9canismes pour convertir les\u2026"
lastmod: '2024-03-13T22:44:57.159240-06:00'
model: gpt-4-0125-preview
summary: "En Go, le package `encoding/json` est votre porte d'entr\xE9e pour la manipulation\
  \ du JSON, offrant des m\xE9canismes pour convertir les structures de donn\xE9es\
  \ Go en JSON (marshalling) et vice versa (unmarshalling)."
title: Travailler avec JSON
weight: 38
---

## Comment faire :
En Go, le package `encoding/json` est votre porte d'entrée pour la manipulation du JSON, offrant des mécanismes pour convertir les structures de données Go en JSON (marshalling) et vice versa (unmarshalling). Voici des exemples de base pour commencer :

### Encodage (Marshalling)
Pour convertir une structure Go en JSON, vous pouvez utiliser `json.Marshal`. Considérez la structure Go suivante :

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type Utilisateur struct {
    ID        int      `json:"id"`
    NomUtilisateur  string   `json:"username"`
    Langages []string `json:"languages"`
}

func main() {
    utilisateur := Utilisateur{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    utilisateurJSON, err := json.Marshal(utilisateur)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(utilisateurJSON))
}
```

Sortie :

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Décodage (Unmarshalling)
Pour analyser du JSON dans une structure de données Go, utilisez `json.Unmarshal` :

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var utilisateur Utilisateur
    err := json.Unmarshal([]byte(jsonStr), &utilisateur)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", utilisateur)
}
```

Étant donné la structure `Utilisateur` comme auparavant, ce code analyse la chaîne JSON en une instance Utilisateur.

Sortie :

```go
{ID:1 NomUtilisateur:JohnDoe Langages:[Go JavaScript Python]}
```

## Plongée Profonde
Le package `encoding/json` en Go propose une API simple qui abstrait une grande partie de la complexité impliquée dans la manipulation JSON. Introduit dès les débuts du développement de Go, ce package reflète la philosophie de Go de simplicité et d'efficacité. Cependant, l'utilisation de la réflexion par `encoding/json` pour inspecter et modifier les structures à l'exécution peut conduire à des performances sous-optimales dans les scénarios intensifs en CPU.

Des alternatives comme `json-iterator/go` et `ffjson` ont émergé, offrant un traitement JSON plus rapide en générant du code statique de marshalling et unmarshalling. Néanmoins, `encoding/json` reste le package le plus utilisé en raison de sa simplicité, sa robustesse et le fait qu'il fait partie de la bibliothèque standard, assurant compatibilité et stabilité à travers les versions de Go.

Malgré sa performance relative plus lente, la facilité d'utilisation et l'intégration avec le système de typage de Go rendent `encoding/json` adapté pour la plupart des applications. Pour ceux qui travaillent dans des contextes où la performance est primordiale, explorer des bibliothèques externes peut valoir la peine, mais pour beaucoup, la bibliothèque standard offre le bon équilibre entre vitesse, simplicité et fiabilité.
