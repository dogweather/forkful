---
title:                "Elm: Calcul d'une date dans le futur ou dans le passé."
simple_title:         "Calcul d'une date dans le futur ou dans le passé."
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elm, vous savez probablement déjà à quel point ce langage est puissant et élégant. Mais saviez-vous qu'il peut également être utilisé pour calculer des dates dans le futur ou le passé ? Dans cet article, nous allons découvrir pourquoi et comment le faire.

## Comment faire

Pour calculer une date dans le futur ou le passé en Elm, nous allons utiliser la fonction `add` de la bibliothèque `Time`. Cette fonction prend deux arguments : un entier représentant le nombre d'unités à ajouter et une unité de temps (jour, mois, année, etc.).

Voici un exemple de code pour calculer une date dans le futur en ajoutant 3 mois à la date actuelle :

```Elm
import Time exposing (..)

dateFutur = add 3 Months now
```

Et voici un autre exemple pour calculer une date dans le passé en soustrayant 2 semaines à la date actuelle :

```Elm
import Time exposing (..)

datePasse = add -2 Weeks now
```

Notez que la fonction `now` nous donne la date et l'heure actuelles en tant qu'objet `Time`. Vous pouvez également spécifier une date et une heure spécifiques en utilisant la fonction `fromPosix` de la bibliothèque `Time`. 

## Plongée en profondeur

La fonction `add` utilise l'arithmétique modulaire pour calculer la date future ou passée. Cela signifie que si vous ajoutez 3 mois à une date, si cette date est le 31 janvier par exemple, le résultat sera le 30 avril car il n'y a pas de 31 avril.

De plus, la fonction `add` renvoie un objet `Time` qui peut être utilisé avec d'autres fonctions de manipulation de dates, telles que `toText` pour afficher la date dans un format spécifique.

Il est également possible de combiner plusieurs appels de la fonction `add` pour calculer des dates plus complexes. Par exemple, pour calculer une date dans 1 an et 4 mois, vous pouvez utiliser le code suivant :

```Elm
import Time exposing (..)

date = add 1 Years (add 4 Months now)
```

## Voir aussi

Pour plus d'informations sur la fonction `add` et la manipulation des dates en général, vous pouvez consulter la documentation officielle d'Elm sur les dates et la bibliothèque `Time`.

- Documentation officielle d'Elm sur les dates : https://package.elm-lang.org/packages/elm/time/latest/Time
- Documentation de la bibliothèque Time : https://package.elm-lang.org/packages/elm/time/latest/Time#add