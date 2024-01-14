---
title:    "Python: Calcul d'une date dans le futur ou le passé"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou dans le passé peut être utile dans de nombreuses situations, notamment pour la gestion de projets, la planification d'événements ou simplement pour avoir une meilleure compréhension du temps.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en utilisant Python, vous pouvez utiliser le module `datetime` intégré. Tout d'abord, vous devez importer ce module dans votre code en utilisant la commande `import datetime`.

Pour calculer une date dans le futur, vous pouvez utiliser la méthode `timedelta(days=x)` où `x` est le nombre de jours à ajouter à la date actuelle. Par exemple, pour calculer la date dans 10 jours à partir d'aujourd'hui, vous pouvez utiliser le code suivant :

```Python
import datetime

aujourdhui = datetime.date.today()
date_futur = aujourdhui + datetime.timedelta(days=10)
print(date_futur)
```

Cela affichera la date dans 10 jours à partir d'aujourd'hui. En utilisant la même méthode, vous pouvez également calculer une date dans le passé en soustrayant un certain nombre de jours.

## Plongée en profondeur

Il est également possible de calculer des dates en utilisant d'autres unités de temps telles que les heures, les minutes et les secondes. De plus, le module `datetime` offre des fonctionnalités avancées telles que le calcul de la différence entre deux dates, la création de nouvelles dates en utilisant des valeurs spécifiques et bien plus encore.

## Voir aussi

- [Documentation sur le module datetime](https://docs.python.org/fr/3/library/datetime.html)
- [Tutoriel sur le module datetime en français](https://zestedesavoir.com/tutoriels/582/le-module-datetime-en-python/)