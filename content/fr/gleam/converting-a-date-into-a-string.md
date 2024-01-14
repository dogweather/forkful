---
title:                "Gleam: Transformer une date en chaîne de caractères"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous travaillez avec des dates dans votre code, il peut être utile de les convertir en chaînes de caractères pour faciliter leur manipulation et leur affichage. Cela peut être particulièrement utile lorsque vous travaillez avec des données de calendrier ou que vous souhaitez formater des dates de manière spécifique pour correspondre à des exigences de présentation particulières.

# Comment faire

La conversion d'une date en chaîne de caractères est assez simple en utilisant le langage de programmation Gleam. Dans l'exemple ci-dessous, nous utiliserons la fonction `Format` pour convertir une date en une chaîne de caractères au format `Mois année` (Month year) :

```
Gleam
import gleam/time

let month_year = time.Format
    .month_year
    { month : time.Month.April, year : 2021 }
    # "Avril 2021"
```

Nous pouvons également utiliser d'autres formats prédéfinis tels que `date_time`, `short_date` ou `long_date` pour obtenir des résultats différents. Vous pouvez également créer votre propre format personnalisé en utilisant les options de `time.Format`.

# Plongeons plus profondément

En utilisant la fonction `Format`, il est également possible de formater une date en utilisant des spécificateurs de conversion tels que `a` pour le jour de la semaine abrégé en trois lettres, `A` pour le jour de la semaine complet, `d` pour le jour du mois, `m` pour le mois en chiffres et bien plus encore. Cela permet une personnalisation encore plus poussée de la chaîne de caractères résultante.

Vous pouvez également utiliser la fonction `DateTime.toString` pour convertir une date en une chaîne de caractères au format ISO 8601. Cela peut être particulièrement utile pour stocker et échanger des dates avec des systèmes externes.

# Voir également

- Documentation Gleam sur les formats et spécificateurs de conversion : [https://gleam.run/modules/gleam_time/latest/time.html#Format](https://gleam.run/modules/gleam_time/latest/time.html#Format)
- Article de blog sur la manipulation des dates en Gleam : [https://bloggle.run/dates-in-gleam/](https://bloggle.run/dates-in-gleam/)
- Exemples de code pour la manipulation des dates en Gleam : [https://github.com/gleam-lang/gleam_stdlib/blob/master/src/time/tests/tests.gleam](https://github.com/gleam-lang/gleam_stdlib/blob/master/src/time/tests/tests.gleam)