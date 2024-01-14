---
title:    "Gleam: Concaténation de chaînes de caractères"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une technique fondamentale en programmation qui permet de combiner plusieurs chaînes de caractères en une seule. Cela peut être utile pour créer des messages dynamiques, formater des données ou générer des identifiants uniques.

## Comment faire

La syntaxe pour concaténer des chaînes de caractères en Gleam est très simple. Il suffit d'utiliser l'opérateur `++` entre les différentes chaînes que vous souhaitez concaténer. Voici un exemple :

```Gleam
concatenation = "Bonjour" ++ " " ++ "Monde!"
```

Lorsque vous exécutez ce code, la valeur de la variable `concatenation` sera "Bonjour Monde!". Vous pouvez également concaténer des variables contenant des chaînes de caractères :

```Gleam
prenom = "Jean"
nom = "Dupont"

nom_complet = prenom ++ " " ++ nom
```

Dans cet exemple, la valeur de la variable `nom_complet` sera "Jean Dupont". Il est également possible de concaténer des chaînes de caractères avec d'autres types de données, tels que des entiers ou des booléens. Les règles de conversion automatique s'appliquent dans ce cas.

## Approfondissement

En utilisant l'opérateur `++`, vous pouvez concaténer des chaînes de caractères de longueurs différentes. Cependant, il est important de noter que cette opération peut devenir inefficace lorsque vous concaténez un grand nombre de chaînes. En effet, chaque fois que vous concaténez une nouvelle chaîne, une nouvelle chaîne complète est créée en mémoire.

Pour de meilleurs résultats, il est recommandé d'utiliser la fonction `String.concat` qui concatène une liste de chaînes en une seule opération. Vous pouvez également utiliser les expressions de chaîne de caractères en utilisant le symbole `~` pour éviter la création de nouvelles chaînes en mémoire.

## Voir aussi

- [Documentation sur la concaténation de chaînes en Gleam](https://gleam.run/documentation/std-lib-string/#concat)
- [Guide de référence de Gleam pour les opérateurs](https://gleam.run/documentation/guide/operators/)
- [Exemples de code pour la manipulation de chaînes en Gleam](https://github.com/gleam-lang/example-code/tree/main/string-manipulation)