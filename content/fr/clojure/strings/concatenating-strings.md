---
date: 2024-01-20 17:34:30.479902-07:00
description: "Comment faire : Historiquement, la concat\xE9nation de cha\xEEnes est\
  \ aussi vieille que la programmation elle-m\xEAme. Dans Clojure, `str` est notre\
  \ outil de base\u2026"
lastmod: '2024-04-05T21:53:58.854335-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, la concat\xE9nation de cha\xEEnes est aussi vieille que\
  \ la programmation elle-m\xEAme."
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## Comment faire :
```Clojure
;; Utilisons `str` pour concaténer des chaînes
(defn salut-monde []
  (str "Bonjour" ", " "monde!"))

(salut-monde) ; Résultat : "Bonjour, monde!"

;; On peut aussi concaténer avec `clojure.string/join`
(defn liste-en-chaine [liste]
  (clojure.string/join ", " liste))

(liste-en-chaine ["pommes" "bananes" "cerises"]) ; Résultat : "pommes, bananes, cerises"
```

## Exploration approfondie
Historiquement, la concaténation de chaînes est aussi vieille que la programmation elle-même. Dans Clojure, `str` est notre outil de base — simple et efficace. Il convertit les arguments en chaînes et les joint. 

Une alternative à `str` est `clojure.string/join`, qui est pas mal quand on a une collection d'éléments à raccorder avec un délimiteur spécifique.

Pour ce qui est du détail implémentation, `str` utilise une StringBuilder Java en sous-main pour optimiser la construction de la nouvelle chaîne, tandis que `clojure.string/join` se sert du `StringJoiner` de Java 8 si disponible, ou alors itère la collection et utilise `str`.

## Voir aussi
- Plus sur `StringJoiner` de Java : [Oracle Docs](https://docs.oracle.com/javase/8/docs/api/java/util/StringJoiner.html)
