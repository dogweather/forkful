---
date: 2024-01-20 17:34:30.479902-07:00
description: "Concat\xE9ner des cha\xEEnes, c'est coller ensemble des bouts de texte\
  \ pour en faire un seul morceau. En programmation, \xE7a sert souvent \xE0 assembler\
  \ des donn\xE9es\u2026"
lastmod: 2024-02-19 22:05:16.172793
model: gpt-4-1106-preview
summary: "Concat\xE9ner des cha\xEEnes, c'est coller ensemble des bouts de texte pour\
  \ en faire un seul morceau. En programmation, \xE7a sert souvent \xE0 assembler\
  \ des donn\xE9es\u2026"
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Concaténer des chaînes, c'est coller ensemble des bouts de texte pour en faire un seul morceau. En programmation, ça sert souvent à assembler des données pour les afficher ou les enregistrer.

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
