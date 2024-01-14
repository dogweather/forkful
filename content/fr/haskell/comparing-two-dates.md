---
title:    "Haskell: Comparer deux dates"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Haskell, vous savez probablement qu'il y a une multitude de bibliothèques pour manipuler les dates et les heures. Mais pourquoi voudriez-vous comparer deux dates en premier lieu ? La réponse est simple : la comparaison de dates peut être utile pour vérifier l'ordre chronologique d'événements ou pour filtrer des données basées sur une plage de temps.

## Comment faire

La comparaison de dates en Haskell est facile grâce à la bibliothèque "Data.Time". Tout d'abord, importez la bibliothèque dans votre code en utilisant la ligne suivante :

```Haskell
import Data.Time
```

Ensuite, vous pouvez utiliser la fonction "diffDays" pour comparer deux dates et obtenir le nombre de jours entre les deux. Par exemple, si vous voulez comparer les dates "19/10/2020" et "25/10/2020", vous pouvez le faire avec le code suivant :

```Haskell
diffDays (fromGregorian 2020 10 19) (fromGregorian 2020 10 25)
```

Le résultat sera "6" car il y a 6 jours entre les deux dates. Vous pouvez également utiliser d'autres fonctions telles que "compare" pour vérifier si une date est avant, après ou égale à une autre date.

## Plongée profonde

Maintenant que vous savez comment comparer des dates en Haskell, vous pouvez vous demander comment la bibliothèque "Data.Time" effectue réellement la comparaison. Sans entrer dans les détails techniques, sachez que la bibliothèque utilise un type de données appelé "Day", qui représente un jour dans le calendrier grégorien. En utilisant ce type de données, la bibliothèque peut facilement effectuer des calculs sur les dates et les comparer.

## Voir aussi

- Documentation officielle de la bibliothèque "Data.Time" : https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Tutoriel sur la manipulation de dates en Haskell : https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#data-time