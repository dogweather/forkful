---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi et Quoi ?

Les expressions régulières, ou RegEx, sont des séquences de caractères formant un modèle de recherche. Les programmeurs s'en servent pour manipuler du texte, chercher et remplacer des fragments de chaînes de caractères.

## Comment faire :

Voyons comment utiliser les expressions régulières en Clojure. 

Pour rechercher une expression dans une chaîne :

```Clojure
(re-find #"[a-z]+" "Hello World!")
; => "ello"
```

Pour remplacer un fragment dans une chaîne :

```Clojure
(clojure.string/replace "Hello World!" #"[a-z]+" "Bonjour")
; => "HBonjour WBonjour!"
```
Pour diviser une chaîne par une expression :

```Clojure
(clojure.string/split "Hello World!" #"\s")
; => ["Hello" "World!"]
```

## Exploration Profonde :
Les expressions régulières sont apparues pour la première fois avec l'invention de l'Unix, dans les années 70. Elles sont maintenant utilisées dans presque tous les langages de programmation. 

Même si les expressions régulières sont très utiles, elles ne sont pas toujours le meilleur outil pour le travail. Parfois il serait plus approprié d’utiliser un analyseur de syntaxe ou même une simple fonction de chaîne.

En Clojure, les expressions régulières sont implémentées comme des littéraux de motif, qui sont des instances de java.util.regex.Pattern. 

## À Voir Aussi :

Voici quelques sources supplémentaires pour explorer davantage les expressions régulières en Clojure :

- [Clojure - Regular Expressions](https://www.tutorialspoint.com/clojure/clojure_regular_expressions.htm)
- [Regular Expressions in Clojure](https://lispcast.com/clojure-regex/)
- [Clojure Documentation - Patterns](https://clojuredocs.org/clojure.core/re-pattern)