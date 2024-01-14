---
title:                "Clojure: Convertir une chaîne en minuscules"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertir une chaîne de caractères en minuscules peut être utile lorsque vous travaillez avec du texte en informatique. Cela peut vous aider à égaliser les données pour les comparer, à formater les entrées utilisateur ou simplement à harmoniser l'affichage du texte. Dans cet article, nous allons voir comment convertir une chaîne de caractères en minuscules en utilisant Clojure.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en Clojure, nous pouvons utiliser la fonction `lower-case` du package `clojure.string`. Voici un exemple de code :

```Clojure
(require '[clojure.string :as str])

(str/lower-case "HELLO WORLD") 
=> "hello world"
```

Dans cet exemple, nous avons d'abord importé le package `clojure.string` et nous avons utilisé la fonction `lower-case` pour convertir la chaîne de caractères "HELLO WORLD" en minuscules. Le résultat renvoyé est "hello world".

Nous pouvons également utiliser `map` pour appliquer la fonction `lower-case` à une liste de chaînes de caractères :

```Clojure
(map str/lower-case ["This" "Is" "A" "TEST"]) 
=> ("this" "is" "a" "test")
```

## Plongée en profondeur

Il est important de noter que la fonction `lower-case` ne fonctionne que sur les caractères ASCII. Pour les caractères non ASCII, il est recommandé d'utiliser la bibliothèque `clojure.java.string` et sa fonction `lower-case` qui prend en charge les caractères Unicode.

De plus, la fonction `lower-case` ne modifie pas la chaîne de caractères d'origine, elle renvoie plutôt une nouvelle chaîne de caractères en minuscules.

## Voir aussi

- [Documentation officielle de `clojure.string`](https://clojuredocs.org/clojure.string)
- [Documentation de `clojure.java.string`](https://clojuredocs.org/clojure.java.string/lower-case)