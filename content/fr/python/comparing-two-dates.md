---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Comparer deux dates en programmation permet de déterminer quelle date est plus récente, plus ancienne, ou si les deux sont identiques. Les programmeurs utilisent cette pratique pour trier des événements, reconnaître des patrons, ou simplement pour gérer le temps d'une manière plus structurée.

## Comment faire :

Utilisons le module `datetime` de Python pour comparer deux dates. 

```Python
from datetime import datetime

# Créer deux dates
date_1 = datetime(2022, 1, 1)
date_2 = datetime(2023, 1, 1)

# Comparaison
if date_1 < date_2:
    print("date_1 est plus ancienne que date_2")
elif date_1 == date_2:
    print("date_1 est la même que date_2")
else:
    print("date_1 est plus récente que date_2")
```

La sortie pour le code ci-dessus sera : `date_1 est plus ancienne que date_2`.

## Exploration en profondeur :

1. Contexte historique : L'objet `datetime` de Python a été introduit dans la version 2.3 et a depuis été utilisé comme la principale manière de gérer les dates et les temps.
2. Alternatives : Bien que `datetime` soit une excellente façon de comparer les dates, il existe également d'autres bibliothèques comme `Pandas` et `NumPy` qui peuvent aider à manipuler et comparer les dates.
3. Détails de mise en œuvre : Le module `datetime` de Python stocke les informations de date et d'heure dans un objet. Quand vous comparez ces objets, Python compare ces informations en secondes depuis une constante appelée "l'époque" qui a été définie comme le 1er janvier 1970.

## Voir également :

1. [Documentation Python sur datetime:](https://docs.python.org/fr/3/library/datetime.html)
2. [Comparaison de dates avec Pandas:](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Timestamp.html)
3. [Comparaison de dates avec NumPy:](https://numpy.org/doc/stable/reference/arrays.datetime.html)
4. [Compréhension de l'époque et du Temps Unix:](https://www.guru99.com/date-time-and-datetime-classes-in-python.html)