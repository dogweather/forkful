---
title:                "Mettre une chaîne en majuscules"
html_title:           "Clojure: Mettre une chaîne en majuscules"
simple_title:         "Mettre une chaîne en majuscules"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

------

## Quoi & Pourquoi ?

Capitaliser une chaîne de caractères, c'est transformer chaque première lettre d'un mot en majuscule. Les programmeurs le font généralement pour faciliter la lecture ou pour suivre les conventions stylistiques.

------

## Comment faire :

Capitalisation de base :

```Clojure 
(clojure.string/capitalize "bonjour tout le monde")
```
Sortie :
```Clojure 
"Bonjour tout le monde"
```

Capitalisation de chaque mot :

```Clojure 
(->> "bonjour tout le monde"
     (clojure.string/split #" ")
     (map clojure.string/capitalize)
     (clojure.string/join " "))
```

Sortie :
```Clojure 
"Bonjour Tout Le Monde"
```

------

## Plongée en profondeur :

La fonction `clojure.string/capitalize` convertit le premier caractère de la chaîne en majuscule et les caractères restants en minuscules. Historiquement, cela fait partie des opérations de base sur les chaînes de caractères dans de nombreux langages de programmation.

Alternativement, on pourrait utiliser Java interopérabilité pour capitaliser une chaîne :

```Clojure 
(.toUpperCase "bonjour tout le monde")
```

Cependant, cette opération ne capitalise pas chaque mot de la chaîne, seulement la première lettre.

Note : L'opération de capitalisation dans Clojure n'est pas locale (elle ne tient pas compte de la langue utilisée). Si vous avez besoin d'une capitalisation sensible à la langue, il est recommandé d'utiliser l'interopérabilité Java avec `java.text.Collator`.

------

## Voir aussi :

1. [Documentation officielle de Clojure](https://clojure.org/reference/strings)
2. [Guide de programmation de Clojure pour les opérations sur les chaînes de caractères](https://clojure.org/guides/learn/strings)
3. [API Java pour la manipulation en majuscules/minuscules de la chaîne](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())