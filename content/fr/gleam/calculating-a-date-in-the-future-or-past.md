---
title:    "Gleam: Calculer une date dans le futur ou le passé"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez en Gleam, il est parfois nécessaire de calculer une date dans le futur ou dans le passé. Que vous souhaitiez planifier une tâche à une certaine date ou simplement afficher la date d'hier, il est important de savoir comment le faire de manière efficace.

## Comment faire

Voici un exemple de code en Gleam pour calculer une date dans le futur en utilisant la bibliothèque «date_utils» :

```Gleam
import date_utils

let today = date_utils.now()

let tomorrow = date_utils.add_days(today, 1)
```

Dans cet exemple, nous importons tout d'abord la bibliothèque «date_utils». Ensuite, nous définissons une variable pour la date actuelle en utilisant la fonction "now()". Enfin, nous utilisons la fonction "add_days()" pour ajouter un jour à la date actuelle et stocker le résultat dans une variable "tomorrow".

Voici un autre exemple pour calculer la date d'hier :

```Gleam
import date_utils

let today = date_utils.now()

let yesterday = date_utils.add_days(today, -1)
```

Comme vous pouvez le constater, il suffit de passer un nombre négatif à la fonction "add_days()" pour calculer une date dans le passé.

## Plongeons plus profondément

En utilisant la bibliothèque "date_utils", il est possible de calculer des dates dans n'importe quel intervalle de temps. Par exemple, vous pouvez ajouter ou soustraire des mois, des années, des heures, etc. en utilisant les fonctions appropriées.

Il est également possible de comparer des dates en utilisant les fonctions "is_before()" et "is_after()". Cela peut être utile pour vérifier si une date est avant ou après une autre, par exemple pour planifier des événements futurs.

## Voir aussi

- La documentation officielle de la bibliothèque "date_utils" pour plus d'informations sur les fonctions disponibles : [lien vers la documentation](https://gleam.run/docs/libraries/date_utils)
- Un tutoriel détaillé sur la manipulation de dates en Gleam : [lien vers le tutoriel](https://exemple.com/tutoriel-dates-gleam)
- Un article sur les bonnes pratiques de gestion des dates en programmation : [lien vers l'article](https://exemple.com/gestion-dates-programmation)