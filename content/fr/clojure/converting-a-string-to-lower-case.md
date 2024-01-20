---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?

La conversion d'une chaîne en minuscules est une opération consistant à transformer toutes les lettres majuscules en lettres minuscules. Les programmeurs le font souvent pour normaliser les données et éviter les problèmes de correspondance de casse.

## Comment faire :

En Clojure, la fonction `lower-case` fera le travail pour vous. Voyez par vous-même:

```clojure
(defn lower-case-string [s]
  (.toLowerCase s))

(lower-case-string "BONJOUR, MON AMI !")
```
Cela donnera en sortie : `"bonjour, mon ami !"`. Comme vous pouvez le voir, toutes les lettres majuscules sont devenues des minuscules.

## Plongée profonde

Historiquement, la conversion en minuscules est une pratique issue de l'époque où les systèmes de fichiers et les bases de données étaient sensibles à la casse. Aujourd'hui, même si ce n'est plus souvent le cas, normaliser en minuscule est une bonne habitude pour éviter des problèmes inattendus.

Il existe des alternatives à `lower-case`, par exemple, vous pouvez utiliser une combinaison de `map` et `char-downcase` dans Clojure. Mais généralement, `lower-case` est plus simple et claire.

En interne, `lower-case` fonctionne en appelant la méthode `toLowerCase()` de Java sur la chaîne. Cette méthode utilise les règles de casse du Locale par défaut de la machine virtuelle Java.

## Voir aussi

1. La documentation Clojure sur `lower-case`: [https://clojuredocs.org/clojure.string/lower-case](https://clojuredocs.org/clojure.string/lower-case)
2. Java `toLowerCase()` : [https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)