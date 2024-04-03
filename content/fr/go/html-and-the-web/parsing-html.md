---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:51.113839-07:00
description: "Comment faire : Pour analyser le HTML en Go, vous utilisez g\xE9n\xE9\
  ralement le package `goquery` ou le package `net/html` de la biblioth\xE8que standard.\
  \ Voici\u2026"
lastmod: '2024-03-13T22:44:57.130894-06:00'
model: gpt-4-0125-preview
summary: "Pour analyser le HTML en Go, vous utilisez g\xE9n\xE9ralement le package\
  \ `goquery` ou le package `net/html` de la biblioth\xE8que standard."
title: Analyse Syntaxique de HTML
weight: 43
---

## Comment faire :
Pour analyser le HTML en Go, vous utilisez généralement le package `goquery` ou le package `net/html` de la bibliothèque standard. Voici un exemple de base utilisant `net/html` pour extraire tous les liens d'une page web :

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // Obtenir le document HTML
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Analyser le document HTML
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Fonction pour parcourir récursivement le DOM
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // Parcourir le DOM
    f(doc)
}
```

Exemple de sortie (en supposant que `http://example.com` contient deux liens) :

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

Ce code demande une page HTML, l'analyse et parcourt récursivement le DOM pour trouver et imprimer les attributs `href` de toutes les balises `<a>`.

## Exploration approfondie
Le package `net/html` fournit les bases pour l'analyse du HTML en Go, en mettant en œuvre directement les algorithmes de tokenisation et de construction d'arbres spécifiés par la norme HTML5. Cette approche de bas niveau est puissante mais peut être verbeuse pour des tâches complexes.

En contraste, le package tiers `goquery`, inspiré de jQuery, propose une interface de niveau supérieur qui simplifie la manipulation et le parcours du DOM. Il permet aux développeurs d'écrire du code concis et expressif pour des tâches telles que la sélection d'éléments, l'extraction d'attributs et la manipulation de contenu.

Cependant, la commodité de `goquery` a un coût en termes de dépendance supplémentaire et éventuellement de performances plus lentes en raison de sa couche d'abstraction. Le choix entre `net/html` et `goquery` (ou d'autres bibliothèques d'analyse) dépend des exigences spécifiques du projet, comme le besoin d'optimisation des performances ou la facilité d'utilisation.

Historiquement, l'analyse du HTML en Go a évolué des opérations de chaîne de base à la manipulation sophistiquée d'arbres DOM, reflétant l'écosystème croissant du langage et la demande de la communauté pour des outils robustes de web scraping et d'extraction de données. Malgré les capacités natives, la prévalence de bibliothèques tierces comme `goquery` souligne la préférence de la communauté Go pour un code modulaire et réutilisable. Cependant, pour des applications critiques en termes de performances, les programmeurs pourraient toujours favoriser le package `net/html` ou même recourir aux expressions régulières pour des tâches d'analyse simples, en gardant à l'esprit les risques et limitations inhérents à l'analyse HTML basée sur les expressions régulières.
