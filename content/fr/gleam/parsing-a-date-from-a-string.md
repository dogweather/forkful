---
title:                "Analyser une date depuis une chaîne de caractères"
html_title:           "Gleam: Analyser une date depuis une chaîne de caractères"
simple_title:         "Analyser une date depuis une chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faisons-nous?

Parser une date à partir d'une chaîne de caractères consiste à extraire une date spécifique (jour, mois, année) à partir d'une chaîne de caractères contenant une information temporelle. Les programmeurs font cela pour pouvoir manipuler et utiliser des dates dans leurs programmes, ce qui est souvent nécessaire pour des opérations telles que la comparaison et le tri de données chronologiques.

## Comment le faire:

Pour parser une date à partir d'une chaîne en utilisant Gleam, vous pouvez utiliser la fonction ```Gleam.Date.fromString```, qui prend en entrée une chaîne de caractères et renvoie une date au format ```Gleam.Date```. Voici un exemple:

```
// Déclarer une chaîne contenant une date
let dateString = "2021-10-15"

// Utiliser la fonction Gleam.Date.fromString
let date = Gleam.Date.fromString(dateString)

// Afficher la date
Debug.todo(date)
```

La sortie de ce code sera ```#Gleam.Date<year=2021, month=10, day=15>```, montrant que la date a été correctement extraite de la chaîne.

## Plongée en profondeur:

L'extraction de dates à partir de chaînes de caractères est un problème courant en programmation, étant donné que les dates sont souvent stockées et communiquées sous forme de chaînes. Il existe plusieurs façons alternatives de parser les dates, telles que l'utilisation de bibliothèques ou de fonctions spécifiques à un langage de programmation. L'implémentation de la fonction ```fromString``` dans Gleam utilise un algorithme de parsing basé sur les formats de date ISO et ANSI, afin d'assurer une compatibilité maximale.

## À voir également:

Vous pouvez en savoir plus sur la fonction ```Gleam.Date.fromString``` dans la documentation officielle de Gleam: https://gleam.run/packages/gleam_stdlib/Date.html#fromString. Vous pouvez également découvrir d'autres articles utiles sur Gleam en consultant notre site web et notre communauté en ligne. Happy coding!