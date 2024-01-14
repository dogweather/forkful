---
title:                "Python: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates est une tâche importante dans de nombreux domaines de programmation pour vérifier la chronologie des événements ou pour calculer la durée entre deux moments précis. Cela peut également être utile dans des situations telles que la planification des tâches ou la gestion de projets.

## Comment faire

Pour comparer deux dates en Python, nous pouvons utiliser le module "datetime" qui fournit des fonctions et des classes pour manipuler les dates et les heures. Voici un exemple de code qui compare deux dates et affiche le résultat :

```python
from datetime import date

date1 = date(2020, 4, 15)
date2 = date(2020, 4, 20)

if date1 < date2:
  print("La date 1 est antérieure à la date 2.")
elif date1 == date2:
  print("Les deux dates sont identiques.")
else:
  print("La date 2 est antérieure à la date 1.")
```

Ce code crée deux objets "date" en utilisant les valeurs fournies et les compare en utilisant la syntaxe des conditions. Le résultat affiché sera "La date 1 est antérieure à la date 2.", car le 15 avril 2020 est avant le 20 avril 2020.

## Approfondir

En Python, les dates sont représentées par des objets "date" qui stockent à la fois la date et l'heure. Les opérations de comparaison telles que "<" (inférieur) et "==" (égal) sont utilisées pour comparer les dates en fonction de leur valeur et non de leur format d'affichage. Cela signifie que même si deux dates peuvent être affichées différemment, elles peuvent être équivalentes lorsqu'elles sont comparées.

De plus, le module "datetime" offre d'autres fonctionnalités utiles telles que la conversion de chaînes de caractères en objets "date" et le calcul de la différence entre deux dates en utilisant la méthode ".days". Vous pouvez explorer ces fonctionnalités en consultant la documentation officielle du module.

## Voir aussi

- [Documentation officielle de Python sur le module "datetime"](https://docs.python.org/fr/3/library/datetime.html)
- [Tutoriel sur la manipulation de dates en Python](https://realpython.com/python-datetime/)
- [Exemples de codes pour comparer des dates en Python](https://www.geeksforgeeks.org/python-program-to-compare-two-dates/)