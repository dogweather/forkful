---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Gleam: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Salut les codeurs ! Aujourd'hui, nous allons parler d'un sujet assez répandu en programmation : comment calculer une date dans le futur ou le passé. Si vous êtes curieux de savoir ce que cela signifie et pourquoi les programmeurs le font, lisez la suite !

## Quoi & Pourquoi ?

Calculer une date dans le futur ou le passé, c'est simplement trouver une date qui n'est pas la date actuelle. Les programmeurs le font souvent pour des tâches telles que la planification d'événements ou la manipulation de données historiques.

## Comment faire :

Voici un exemple de code en Gleam pour calculer une date dans le futur :

```
Gleam
import gleam/calendar.Calendar

pub fn main() {
  let now = Calendar.now()
  let future = Calendar.from_now(now, 10, "days")
  debug!(future)
}
```

Le code ci-dessus utilise une bibliothèque de calendrier pour trouver la date actuelle, puis ajoute 10 jours à cette date pour obtenir une date dans le futur. Vous pouvez également utiliser la fonction `from_ago` pour trouver une date dans le passé en spécifiant un nombre négatif de jours.

Voici une sortie d'exemple pour ce code :

```
2020-08-20T08:44:27.713Z
```

## Plongée profonde :

Bien que calculer des dates dans le futur ou le passé puisse sembler simple, cela peut être compliqué en fonction de la façon dont vous voulez manipuler les données temporelles. Si vous cherchez des alternatives à Gleam, certaines options populaires sont MomentJS et Joda Time.

Pour les détails de mise en œuvre, vous pouvez creuser dans la logique derrière les bibliothèques de calendrier et voir comment elles calculent les dates à partir d'un point de départ donné.

## Voir aussi :

Si vous voulez en savoir plus sur le calcul de dates dans le futur ou le passé, voici quelques liens utiles :

- [Documentation Gleam pour les fonctions de calendrier] (https://gleam.run/doc/gleam/calendar)
- [MomentJS] (https://momentjs.com/)
- [Joda Time] (https://www.joda.org/joda-time/)

Et voilà ! Maintenant vous savez comment calculer des dates dans le futur ou le passé en utilisant Gleam. Amusez-vous bien à coder et n'hésitez pas à explorer d'autres fonctionnalités de Gleam pour améliorer encore plus vos compétences en programmation. À la prochaine !