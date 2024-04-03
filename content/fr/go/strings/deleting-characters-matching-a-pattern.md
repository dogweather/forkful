---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:29.056773-07:00
description: "Comment faire : En Go, supprimer les caract\xE8res correspondant \xE0\
  \ un mod\xE8le peut \xEAtre accompli efficacement en utilisant le package `regexp`.\
  \ Ici, nous\u2026"
lastmod: '2024-03-13T22:44:57.114690-06:00'
model: gpt-4-0125-preview
summary: "En Go, supprimer les caract\xE8res correspondant \xE0 un mod\xE8le peut\
  \ \xEAtre accompli efficacement en utilisant le package `regexp`."
title: "Supprimer des caract\xE8res correspondant \xE0 un motif"
weight: 5
---

## Comment faire :
En Go, supprimer les caractères correspondant à un modèle peut être accompli efficacement en utilisant le package `regexp`. Ici, nous montrerons comment supprimer tous les chiffres, puis tous les caractères non alphanumériques d'une chaîne de caractères comme exemples.

1. **Supprimer tous les chiffres :**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go1 est cool, mais Go2 sera plus cool ! Maintenant : 2023."
	
    // Compiler l'expression régulière pour les chiffres
    re, err := regexp.Compile("[0-9]+")
    if err != nil {
        fmt.Println("Erreur lors de la compilation de l'expression régulière :", err)
        return
    }
	
    // Remplacer les chiffres par une chaîne vide
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // Sortie : Go est cool, mais Go sera plus cool ! Maintenant : .
}
```

2. **Supprimer tous les caractères non alphanumériques :**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go est n°1 @ langages de programmation !"
	
    // Compiler l'expression régulière pour les caractères non alphanumériques
    re, err := regexp.Compile("[^a-zA-Z0-9]+")
    if err != nil {
        fmt.Println("Erreur lors de la compilation de l'expression régulière :", err)
        return
    }
	
    // Remplacer les caractères non alphanumériques par une chaîne vide
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // Sortie : Goestn1langagesdeprogrammation
}
```

## Approfondissement
Le package `regexp` en Go offre une interface puissante pour la correspondance de motifs et la manipulation avec des expressions régulières. Son implémentation est dérivée de RE2, une bibliothèque d'expressions régulières conçue pour garantir une exécution en temps linéaire, évitant la possibilité de problèmes de "retour en arrière catastrophique" présents dans certains autres moteurs d'expressions régulières. Cela rend les regex de Go relativement sûrs et efficaces pour un large éventail d'applications.

Bien que le package `regexp` soit une solution complète pour traiter des motifs, il convient de noter que pour des manipulations de chaînes plus simples ou hautement spécifiques, d'autres fonctions de chaîne comme `strings.Replace()`, `strings.Trim()`, ou le découpage peuvent offrir des alternatives plus performantes. Les expressions régulières sont un outil puissant, mais leur coût computationnel relatif signifie que pour des opérations qui peuvent être spécifiées sans elles, explorer des alternatives de la bibliothèque standard peut parfois conduire à un code plus simple et plus efficace.
