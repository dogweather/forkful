---
title:                "Haskell: La concaténation de chaînes"
simple_title:         "La concaténation de chaînes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une pratique courante en programmation pour combiner des chaînes de caractères différentes en une seule. Cela peut être utile pour créer des messages dynamiques ou pour manipuler des données provenant de différentes sources.

## Comment faire

La concaténation de chaînes de caractères en Haskell se fait à l'aide de l'opérateur « ++ ». Voici un exemple de code montrant comment concaténer deux chaînes :

```Haskell
message = "Bonjour "
name = "Marie"
greeting = message ++ name  -- Résultat: "Bonjour Marie"
```

La fonction « ++ » peut également prendre en charge plus de deux chaînes à la fois. Elle peut également être utilisée pour concaténer des chaînes avec d'autres types de données, comme des entiers :

```Haskell
age = 25
message = "J'ai " ++ age ++ " ans."  -- Résultat: "J'ai 25 ans."
```

Il est important de noter que les variables doivent être du même type de données pour être concaténées. Par exemple, il n'est pas possible de concaténer une chaîne de caractères avec un entier directement.

## Plongée en profondeur

En Haskell, les chaînes de caractères sont en fait des listes de caractères. Cela signifie que la concaténation de chaînes utilise en fait la fonction de concaténation de listes. Par conséquent, la concaténation est une opération très efficace en Haskell.

Il est également possible de concaténer des chaînes de caractères en utilisant la fonction « concat » qui prend une liste de chaînes en entrée et les concatène en une seule :

```Haskell
messages = ["Bonjour ", "comment ", "allez-vous ?"]
fullMessage = concat messages  -- Résultat: "Bonjour comment allez-vous ?"
```

De plus, Haskell offre plusieurs autres fonctions pour manipuler des chaînes de caractères, telles que « take », « drop » et « reverse », qui peuvent être combinées avec la concaténation pour créer des chaînes personnalisées selon les besoins.

## Voir aussi

- [Documentation sur la concaténation en Haskell](https://wiki.haskell.org/Introduction_to_Haskell_Strings#Concatenation)
- [Tutoriel sur les chaînes de caractères en Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Exemples de manipulation de chaînes en Haskell](https://www.codementor.io/@drewlustro/working-with-strings-in-haskell-c3y8zl6nf)