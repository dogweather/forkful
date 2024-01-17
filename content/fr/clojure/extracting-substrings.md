---
title:                "Extraction de sous-chaînes"
html_title:           "Clojure: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce & pourquoi?
Extraction de sous-chaînes est le processus de sélection d'une partie d'une chaîne de caractères spécifique, basée sur ses indices ou son contenu. Les programmeurs le font souvent pour manipuler, trier ou formater des données de manière efficace.

## Comment:
### Exemple 1:
```clojure
(println (.substring "Bonjour tout le monde" 8)) ;; Output: "tout le monde"
```
### Exemple 2:
```clojure
(println (.substring "Bonjour tout le monde" 3 7)) ;; Output: "jour"
```

## Plongée en profondeur:
L'extraction de sous-chaînes existe depuis les premiers langages de programmation et continue d'être une fonctionnalité utile dans de nombreux langages, y compris Clojure. Une alternative à l'extraction de sous-chaînes est l'utilisation de expressions régulières, mais cela peut être plus compliqué et moins efficace dans certaines situations. L'implémentation de l'extraction de sous-chaînes dans Clojure utilise la méthode Java `substring()`.

## Voir aussi:
- La documentation officielle de Clojure sur l'extraction de sous-chaînes
- Des tutoriels en ligne sur la manipulation de chaînes de caractères en Clojure