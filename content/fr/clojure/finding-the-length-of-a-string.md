---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Clojure: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes curieux de connaître la taille d'une chaîne de caractères en Clojure, alors cet article est fait pour vous ! De manière générale, savoir trouver la longueur d'une chaîne de caractères peut être utile dans de nombreux cas de programmation, que ce soit pour la manipulation de données ou la vérification de la validité d'une entrée utilisateur.

## Comment faire
Pour trouver la longueur d'une chaîne de caractères en Clojure, on peut utiliser la fonction `count` qui renvoie le nombre de caractères dans une chaîne. Voici un exemple :

```Clojure 
(count "Bonjour") 
```

Cet exemple renverra la valeur 7, car la chaîne "Bonjour" contient 7 caractères.

On peut également utiliser `str` pour créer une chaîne à partir d'une liste de caractères et ainsi obtenir sa longueur. Voici un autre exemple :

```Clojure 
(count (str \C \l \o \j \u \r \e))
```

Cet exemple renvoie également la valeur 7, car la chaîne "Cljoure" contient 7 caractères.

## Plongée en profondeur
Il est important de noter que la fonction `count` peut également être utilisée sur d'autres types de données en Clojure, tels que les listes, les vecteurs ou les ensembles. Elle renverra alors la taille de ces structures de données. Par exemple :

```Clojure 
(count [1 2 3]) ;; renvoie 3 
(count #{:a :b :c}) ;; renvoie 3 
```

De plus, `count` peut également être utilisée en combinaison avec d'autres fonctions en Clojure, telles que `map` ou `reduce`, pour effectuer des opérations sur chaque élément d'une chaîne ou d'une structure de données. Cela permet une grande flexibilité dans la manipulation de données et dans l'obtention de la longueur d'une chaîne.

## Voir aussi
- Documentation officielle de la fonction `count` : https://clojuredocs.org/clojure.core/count
- Article sur la manipulation de chaînes de caractères en Clojure : https://www.baeldung.com/clojure-strings