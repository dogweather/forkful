---
date: 2024-01-20 15:30:39.019288-07:00
description: "Comment faire : Historiquement, l'analyse HTML \xE9tait ardue. La variabilit\xE9\
  \ et la complexit\xE9 du HTML rendaient les parseurs classiques insuffisants.\u2026"
lastmod: '2024-04-05T22:51:11.401149-06:00'
model: unknown
summary: "Historiquement, l'analyse HTML \xE9tait ardue."
title: Analyse syntaxique de HTML
weight: 43
---

## Comment faire :
```clojure
(require '[enlive.core :as enlive])

; Charger le HTML depuis une URL ou un fichier
(def page-html (enlive/html-resource (java.net.URL. "http://exemple.com")))

; Sélectionner et extraire des éléments avec un sélecteur CSS
(defn extract-titles [html]
  (map :content (enlive/select html [:h1])))

; Utiliser sur notre page chargée
(extract-titles page-html)
```

Sortie prévue :

```clojure
("Titre de la Page")
```

## Plongée profonde
Historiquement, l'analyse HTML était ardue. La variabilité et la complexité du HTML rendaient les parseurs classiques insuffisants. EnLive, la bibliothèque utilisée dans notre exemple Clojure, utilise des sélecteurs à la CSS pour identifier les éléments, simplifiant ainsi le processus.

Alternatives :
- `jsoup` est une autre option, similaire à EnLive mais issue du monde Java.
- `hickory` et `hiccup` sont des bibliothèques Clojure pour représenter et manipuler du HTML de manière idiomatique.

Détails d'implémentation :
EnLive navigue dans le DOM (Document Object Model) pour récupérer et manipuler des éléments. L'approche consistant à utiliser des sélecteurs CSS pour identifier les zones d’intérêt rend l'outil à la fois puissant et accessible.

## Voir Aussi
- Documentation de EnLive : [https://github.com/cgrand/enlive](https://github.com/cgrand/enlive)
- Tutoriel jsoup pour les débutants : [https://jsoup.org/cookbook/](https://jsoup.org/cookbook/)
- La documentation de Hickory : [https://github.com/davidsantiago/hickory](https://github.com/davidsantiago/hickory)
- Guide de démarrage rapide Hiccup : [https://github.com/weavejester/hiccup](https://github.com/weavejester/hiccup)
