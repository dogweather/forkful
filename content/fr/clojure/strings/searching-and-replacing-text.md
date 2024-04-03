---
date: 2024-01-20 17:57:46.519593-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:57.266182-06:00'
model: gpt-4-1106-preview
summary: .
title: Recherche et remplacement de texte
weight: 10
---

## How to:
```Clojure
;; Recherche et remplacement simple avec `clojure.string/replace`
(require '[clojure.string :as str])

;; Exemple: Remplacer "chien" par "chat"
(str/replace "J'ai un chien et un chien." "chien" "chat")
;; => "J'ai un chat et un chat."

;; Utilisation d'une expression régulière (regex) pour remplacer toutes les occurrences d'une chiffre par un '#'
(str/replace "Les numéros 12345 sont importants." #"\d" "#")
;; => "Les numéros ##### sont importants."

;; Remplacer avec une fonction de correspondance
(str/replace "Je veux XXL et non L" #"\b[X]+\b" (fn [match] (clojure.string/lower-case match)))
;; => "Je veux xxl et non L"
```

## Deep Dive
Avant, on manipulait de gros blocs de texte manuellement, fatiguant, n’est-ce pas ? Aujourd'hui, avec les langages de programmation comme Clojure, on utilise des fonctions ou des regex. 

Pourquoi regex ? Ils sont puissants, précis, mais peuvent être complexes. Clojure simplifie la tâche avec `clojure.string/replace`, qui cache la complexité des regex.

D'autres options existent, comme `sed` en ligne de commande ou des bibliothèques spécialisées. Clojure même fournit plus de contrôle avec des librairies telles que `re-find`, `re-seq`, `re-matches` pour des cas plus avancés.

L'implémentation dans Clojure est efficace grâce à l'interopérabilité avec Java et sa `java.util.regex` API. Cela rend les opérations de texte robustes et performantes.

## See Also
- [Clojure Docs - `clojure.string/replace`](https://clojuredocs.org/clojure.string/replace)
- [Clojure from the ground up - Regular expressions](https://aphyr.com/posts/305-clojure-from-the-ground-up-regular-expressions)
- [Java Pattern class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
