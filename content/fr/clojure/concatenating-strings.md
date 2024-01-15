---
title:                "Concaténation de chaînes"
html_title:           "Clojure: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous programmez en Clojure, vous avez probablement déjà eu besoin de concaténer des chaînes de caractères. Par exemple, pour afficher un message personnalisé en combinant des variables. Dans cet article, nous allons voir comment réaliser cette opération courante en utilisant la concaténation de chaînes en Clojure.

## Comment faire

La concaténation de chaînes de caractères en Clojure se fait en utilisant l'opérateur "+" ou en utilisant la fonction "str". Voici un exemple de concaténation de deux chaînes de caractères en utilisant l'opérateur "+" :

```Clojure
(+ "Bonjour " "monde") ; renvoie "Bonjour monde"
```

Et voici un exemple en utilisant la fonction "str" :

```Clojure
(str "La valeur de x est " 42) ; renvoie "La valeur de x est 42"
```

Les deux méthodes fonctionnent de la même manière, mais la fonction "str" est plus polyvalente car elle peut concaténer différents types de données (chaînes de caractères, nombres, listes, etc.).

## Plongée en profondeur

En Clojure, la concaténation de chaînes de caractères est en fait réalisée en utilisant la fonction "concat", qui prend en paramètres une ou plusieurs listes et renvoie une liste qui contient la concaténation de tous les éléments de ces listes. La fonction "str" utilise en interne la fonction "concat" pour concaténer les chaînes de caractères.

Il est également important de noter que la concaténation de chaînes de caractères en Clojure est une opération coûteuse en termes de performances, car une nouvelle liste est créée à chaque concaténation. Si vous avez besoin de concaténer un grand nombre de chaînes, il est donc préférable d'utiliser la fonction "str" plutôt que l'opérateur "+".

## Voir aussi

- Documentation officielle de Clojure sur la concaténation de chaînes : https://clojure.org/guides/learn/strings#_concatenation
- Un autre article sur la concaténation de chaînes en Clojure : https://www.baeldung.com/clojure-string-concatenation