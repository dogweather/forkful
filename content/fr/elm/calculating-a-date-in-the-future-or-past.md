---
title:                "Elm: Calculer une date dans le futur ou le passé"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes développeur en Elm, vous avez peut-être rencontré des situations où vous avez besoin de calculer une date dans le futur ou dans le passé. Que ce soit pour afficher un événement à venir ou pour calculer les intérêts sur un prêt, la manipulation de dates est un aspect important de la programmation. Dans cet article, nous verrons comment calculer une date en Elm en utilisant la bibliothèque Time.

# Comment faire

Pour calculer une date dans le futur ou dans le passé en Elm, nous allons utiliser la fonction `add` de la bibliothèque Time. Cette fonction prend en paramètres une durée et une date de référence. Elle renvoie alors la date résultante en ajoutant ou en soustrayant la durée à la date de référence.

```Elm
import Time

-- Calculer une date dans 2 semaines à partir d'aujourd'hui
let futureDate = Time.add (Time.weeks 2) Time.now

-- Calculer une date il y a 3 mois à partir d'aujourd'hui
let pastDate = Time.add (Time.months (-3)) Time.now
```

Dans cet exemple, nous importons d'abord la bibliothèque Time, puis nous utilisons la fonction `add` pour calculer une date dans le futur en ajoutant 2 semaines à la date actuelle. Nous faisons de même pour calculer une date dans le passé, en soustrayant cette fois-ci 3 mois à la date actuelle.

# Plongée profonde

Maintenant que nous savons comment utiliser la fonction `add`, examinons de plus près la manière dont les durées sont traitées en Elm. La bibliothèque Time utilise le type de données `Time.Duration` pour représenter une durée. Ce type de données peut être créé à l'aide de différentes fonctions, telles que `Time.minutes`, `Time.hours`, `Time.days`, etc.

De plus, il est important de noter que `Time.Duration` utilise un système de type fort pour éviter toute confusion entre les différentes unités de temps. Par exemple, vous ne pouvez pas ajouter une durée de 3 minutes à une date en utilisant `Time.add`, car `Time.minutes` accepte uniquement un `Int` en tant que paramètre. Vous devez plutôt convertir la valeur de minutes en secondes en utilisant `Time.seconds`. Ce système de type garantit une manipulation précise des dates et heures en Elm.

# Voir aussi

Pour en savoir plus sur la bibliothèque Time en Elm, consultez la documentation officielle [ici](https://package.elm-lang.org/packages/elm/time/latest/).

Découvrez également comment gérer les fuseaux horaires en Elm en lisant cet article [Comment gérer les fuseaux horaires en Elm](https://dev.to/jessicalevine328/managing-timezones-in-elm-99k).