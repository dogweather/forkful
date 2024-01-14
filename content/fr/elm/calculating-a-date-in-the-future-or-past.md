---
title:    "Elm: Calculer une date dans le futur ou dans le passé"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi

La programmation est un moyen de résoudre des problèmes et de simplifier nos tâches quotidiennes. En utilisant le langage Elm, vous pouvez facilement calculer des dates dans le futur ou dans le passé pour automatiser des tâches telles que la réservation de billets de train ou la planification d'événements.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en Elm, vous pouvez utiliser la fonction `add` de la bibliothèque `Time` en spécifiant l'unité de temps que vous souhaitez ajouter ou soustraire. Par exemple, si vous voulez ajouter 2 jours à la date actuelle, vous pouvez utiliser `Time.add Days 2`. Voici un exemple complet de code :

```Elm
import Time

-- date actuelle
currentDate = Time.now
-- calculer une date dans 2 jours
futureDate = Time.add Days 2 currentDate
-- calculer une date dans 10 minutes
pastDate = Time.add Minutes -10 currentDate
```

La fonction `add` renvoie un objet `Time.Posix` qui représente la date calculée. Cette valeur peut ensuite être formatée en utilisant la fonction `format` de la même bibliothèque.

## Plongée en profondeur

Lorsque vous utilisez la fonction `add` pour calculer une date dans le futur ou dans le passé, il est important de comprendre comment l'heure est gérée en Elm. Le langage prend en compte les fuseaux horaires et stocke les dates en utilisant le nombre de secondes écoulées depuis le 1er janvier 1970. Cela signifie que les dates calculées peuvent varier en fonction du fuseau horaire dans lequel vous vous trouvez.

Il est également important de noter que la bibliothèque `Time` utilise le système de calendrier grégorien pour gérer les dates. Cela peut avoir un impact sur les résultats si vous travaillez avec des dates antérieures à l'adoption du calendrier grégorien en 1582.

## Voir aussi

- La documentation officielle d'Elm sur la bibliothèque `Time` : https://package.elm-lang.org/packages/elm/time/latest/
- Un tutoriel sur la manipulation des dates en Elm : https://levelup.gitconnected.com/how-to-work-with-dates-in-elm-f1ee7e2a8e55
- Un forum de discussion pour poser vos questions sur la programmation en Elm : https://discourse.elm-lang.org/