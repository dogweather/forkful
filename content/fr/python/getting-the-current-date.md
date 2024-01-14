---
title:                "Python: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Obtenir la date actuelle peut sembler trivial, mais c'est un élément essentiel de nombreux programmes Python. Que ce soit pour enregistrer des données, générer des rapports ou simplement afficher l'heure actuelle, connaître la date en temps réel est crucial pour de nombreuses tâches informatiques.

## Comment faire

Il existe plusieurs façons d'obtenir la date actuelle en Python. La méthode la plus courante est d'utiliser le module `datetime` intégré. Voici un exemple de code qui utilise ce module pour afficher la date actuelle :

```Python
import datetime

now = datetime.datetime.now()
print("La date actuelle est :", now)
```

La sortie de ce code sera quelque chose comme :

```Python
La date actuelle est : 2021-06-14 19:15:25.789227
```

Comme vous pouvez le voir, la date est affichée au format année-mois-jour heure:minute:seconde.microseconde. Mais vous pouvez également formater la date selon vos préférences en utilisant la méthode `strftime()` du module `datetime`.

Par exemple, si vous voulez afficher la date au format jj/mm/aaaa, vous pouvez utiliser le code suivant :

```Python
import datetime

now = datetime.datetime.now()
print("La date actuelle est :", now.strftime("%d/%m/%Y"))
```

La sortie sera :

```Python
La date actuelle est : 14/06/2021
```

## Plongée en profondeur

En utilisant le module `datetime`, vous pouvez également obtenir la date d'une manière plus spécifique. Par exemple, vous pouvez utiliser la méthode `date()` pour obtenir uniquement la date sans l'heure et les microsecondes.

De plus, le module `datetime` offre la possibilité de manipuler les dates en effectuant des opérations comme l'addition ou la soustraction de jours, de semaines, de mois ou d'années à une date donnée.

Vous pouvez également utiliser des méthodes telles que `replace()` pour remplacer une partie spécifique d'une date, ou `weekday()` pour obtenir le jour de la semaine correspondant à une date donnée.

# Voir aussi

- [Documentation officielle de Python pour le module datetime](https://docs.python.org/fr/3/library/datetime.html)
- [Tutoriel sur le module datetime en français](https://python.developpez.com/cours/apprendre-python-3/?page=datetime)
- [Autres façons d'obtenir la date et l'heure en Python](https://www.programiz.com/python-programming/datetime)