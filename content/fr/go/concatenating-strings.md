---
title:                "Fusionner des chaînes de caractères"
html_title:           "Go: Fusionner des chaînes de caractères"
simple_title:         "Fusionner des chaînes de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Lorsqu'on programme en Go, on travaille avec différents types de données, tels que les entiers, les booléens, et les chaînes de caractères. Les chaînes de caractères sont des séquences de lettres, de chiffres et de symboles utilisés pour stocker du texte. Lorsque nous voulons combiner plusieurs chaînes de caractères en une seule, on utilise une opération appelée "concaténation de chaînes".

Les programmeurs utilisent la concaténation de chaînes pour créer du texte dynamique ou pour assembler des messages d'erreurs. Elle est également utile pour construire des URL ou tout autre type de texte qui nécessite l'ajout de différents morceaux ensemble.

## Comment faire?

Go propose plusieurs façons de concaténer des chaînes de caractères. Une des façons les plus simples est d'utiliser l'opérateur `+`, qui permet de mettre deux chaînes de caractères côte à côte. Regardez cet exemple:

```Go
prenom := "Jean"
nom := "Dupont"
nomComplet := prenom + " " + nom
fmt.Println(nomComplet)
```

Cela va imprimer "Jean Dupont" dans la console. On peut également utiliser la fonction `fmt.Sprintf` qui permet de formater une chaîne de caractères avec des valeurs variables. Voici un exemple pour concaténer une chaîne de caractères avec un entier:

```Go
message := fmt.Sprintf("Le nombre est %d", 42)
fmt.Println(message)
```

Ce code va imprimer "Le nombre est 42". On peut aussi utiliser la méthode `Join` de la bibliothèque `strings` pour concaténer plusieurs chaînes de caractères en une seule.

## Plongée dans les détails

La concaténation de chaînes de caractères est un concept assez simple mais elle peut avoir un impact important sur les performances de notre code si elle est utilisée de manière intensive. En effet, chaque fois que nous concaténons une chaîne de caractères, une nouvelle chaîne doit être créée en mémoire, ce qui peut affecter les performances de notre programme.

Une alternative à la concaténation de chaînes de caractères est l'utilisation de la fonction `strings.Builder` qui permet de construire une chaîne de caractères en ajoutant des morceaux à partir de chaînes déjà existantes.

En ce qui concerne l'implémentation, Go utilise une bibliothèque optimisée pour la concaténation de chaînes de caractères, ce qui rend cette opération efficace et rapide.

## Voir aussi

Pour en savoir plus sur la concaténation de chaînes de caractères en Go, n'hésitez pas à consulter la documentation officielle de Go sur les chaînes de caractères et la bibliothèque `strings` : https://golang.org/doc/effective_go.html#strings. Vous pouvez également consulter d'autres sources telles que des blogs ou des forums de discussion pour voir comment d'autres programmeurs utilisent la concaténation de chaînes dans leurs projets.