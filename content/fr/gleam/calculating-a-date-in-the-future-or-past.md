---
title:                "Gleam: Calcul d'une date dans le futur ou le passé"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut y avoir plusieurs raisons pour lesquelles quelqu'un voudrait calculer une date dans le futur ou dans le passé en utilisant Gleam. Peut-être avez-vous besoin de planifier un événement futur, ou peut-être voulez-vous vérifier une date antérieure pour vos archives. Quelle que soit la raison, Gleam a de nombreuses fonctions intégrées pour faciliter le calcul de dates.

## Comment Faire

Voici un exemple de code en Gleam pour calculer une date dans le futur ou dans le passé :

```Gleam
import gleam/datetime

// Calculer une date dans le futur
let future = datetime.add(time, duration)

// Calculer une date dans le passé
let past = datetime.sub(time, duration)

// Afficher les dates calculées
io.println("Date dans le futur :", datetime.format(future, "D-M-YYYY"))
io.println("Date dans le passé :", datetime.format(past, "D-M-YYYY"))
```

Voici un exemple de sortie pour cette code :

```
Date dans le futur : 19-12-2022
Date dans le passé : 19-12-2018
```

Vous pouvez également utiliser des fonctions telles que `datetime.add_days()` ou `datetime.sub_weeks()` pour un calcul plus spécifique. Assurez-vous de consulter la documentation Gleam pour plus de détails sur ces fonctions.

## Profondeur

Calculer une date peut sembler assez simple, mais il y a en fait beaucoup de choses qui se passent en coulisses. Les dates sont stockées sous forme de nombres de secondes depuis une date de référence spécifique, généralement le 1er janvier 1970. Le calcul se fait en convertissant ces secondes en un format de date compréhensible pour les humains.

De plus, des choses comme les fuseaux horaires et les années bissextiles doivent également être prises en compte lors du calcul des dates. Heureusement, Gleam s'occupe de toutes ces complexités pour vous, de sorte que vous pouvez vous concentrer sur le code qui vous intéresse.

## Voir aussi

- Documentation Gleam pour la manipulation de dates: [lien vers la documentation](https://gleam.run/documentation/standard_library.html#datetime)
- Tutoriel vidéo sur la manipulation de dates avec Gleam: [lien vers la vidéo](https://www.youtube.com/watch?v=9j4h8whrwDA)
- Blog post sur l'utilisation de dates dans les applications web en utilisant Gleam: [lien vers le blog](https://medium.com/@Gleam/using-dates-in-web-applications-with-gleam-12b75fe847d8)