---
title:                "Trouver la longueur d'une chaîne de caractères."
html_title:           "Clojure: Trouver la longueur d'une chaîne de caractères."
simple_title:         "Trouver la longueur d'une chaîne de caractères."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La longueur d'une chaîne de caractères est simplement le nombre de caractères présents dans cette chaîne. Les programmeurs ont souvent besoin de trouver la longueur d'une chaîne pour diverses raisons telles que la validation de données d'entrée, la manipulation de chaînes de caractères et la mise en forme de texte.

## Comment:

Ci-dessous, vous trouverez un exemple de code en Clojure pour trouver la longueur d'une chaîne de caractères en utilisant la fonction `count` :

```
(def string "Bonjour le monde")
(count string)
```

Lorsque vous exécutez ce code, vous obtenez la sortie suivante :

```
15
```

Cela signifie que la chaîne "Bonjour le monde" a une longueur de 15 caractères.

## Deep Dive:

La fonction `count` de Clojure provient du langage de programmation Lisp, dont Clojure est une variante. Cette fonction existe également dans d'autres langages tels que Python et JavaScript, sous le nom de `len`.

Il existe également d'autres façons de trouver la longueur d'une chaîne en utilisant des boucles et des variables, mais l'utilisation de la fonction `count` est plus concise et efficace.

En termes d'implémentation, la fonction `count` utilise la propriété `length` de la structure de données de la chaîne pour trouver sa longueur.

## Voir aussi:

Pour en savoir plus sur les fonctions de manipulation de chaînes en Clojure, jetez un œil à la documentation officielle : https://clojure.org/guides/strings

Pour une comparaison entre les différentes façons de trouver la longueur d'une chaîne en Clojure, consultez cet article : https://coderwall.com/p/njgwva/counting-string-length-in-clojure

Et si vous voulez voir la version originale de la fonction `count` dans Lisp, voici un lien vers la documentation : http://lisp-lang.org/special_forms.html#function-count

Happy coding!