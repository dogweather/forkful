---
title:                "Python: Comparer deux dates"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates peut sembler être une tâche simple, mais c'est en réalité une compétence très utile pour les développeurs Python. Cela peut être particulièrement utile lorsque vous travaillez avec des données temporelles comme des statistiques ou des calendriers.

## Comment faire

Pour comparer deux dates en Python, il existe plusieurs méthodes à utiliser en fonction de ce que vous souhaitez faire exactement. Voici deux façons courantes de le faire:

```Python
# Utilisation de l'opérateur de comparaison ">" (plus grand que) pour vérifier si la première date est après la deuxième date
if date_1 > date_2:
  print("La date 1 vient après la date 2")
elif date_1 < date_2:
  print("La date 1 vient avant la date 2")
else:
  print("Les deux dates sont identiques")

# Utilisation de la méthode "compare" pour retourner un nombre négatif si la première date est avant la deuxième, un nombre positif si la première date est après la deuxième, et 0 si elles sont identiques
if date_1.compare(date_2) < 0:
  print("La date 1 vient avant la date 2")
elif date_1.compare(date_2) > 0:
  print("La date 1 vient après la date 2")
else:
  print("Les deux dates sont identiques")
```

L'exemple ci-dessus utilise des dates au format standard "année-mois-jour", mais il est possible d'utiliser d'autres formats de dates en utilisant des modules tels que "datetime" ou "calendar".

## Deep Dive

Lorsque vous comparez des dates, il est important de prendre en compte les différents éléments de la date tels que l'année, le mois et le jour. Par exemple, si vous utilisez l'opérateur ">", cela signifie que la première date doit être strictement supérieure à la deuxième, donc elle doit avoir une année différente OU un mois différent OU un jour différent pour être considérée comme étant après la deuxième date. Si vous souhaitez simplement vérifier si la première date est après la deuxième sans tenir compte des éléments individuels, vous pouvez utiliser la méthode "date2 > date1".

Il peut également être utile de comparer des dates en utilisant des intervalles de temps plutôt que des dates spécifiques. Par exemple, vous pouvez utiliser la méthode "date.today()" pour obtenir la date actuelle, puis utiliser la méthode "timedelta" pour créer un intervalle de temps et comparer les dates en fonction de cet intervalle.

## Voir aussi

- [Documentation Python sur les dates et heures](https://docs.python.org/fr/3/library/datetime.html)
- [Article de programmation sur les opérateurs de comparaison en Python](https://realpython.com/python-operators-expressions/#comparison-operators)
- [Tutoriel sur l'utilisation des dates en Python](https://www.geeksforgeeks.org/date-time-programs-python-set-2-date-class/)