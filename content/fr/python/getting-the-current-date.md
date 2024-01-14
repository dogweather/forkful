---
title:                "Python: Obtenir la date actuelle"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi Obtenir la Date Actuelle Avec Python ?

Si vous êtes un programmeur Python, vous avez peut-être déjà eu besoin de récupérer la date actuelle dans l'un de vos projets. Cela peut sembler une chose simple et triviale, mais connaître la date actuelle peut être très utile dans de nombreuses situations, telles que l'automatisation de tâches, la gestion de données et bien plus encore. Dans cet article, nous allons voir comment obtenir la date actuelle en utilisant Python.

## Comment le Faire

Pour obtenir la date actuelle en utilisant Python, nous allons utiliser le module `datetime`. Ce module contient de nombreuses fonctions pour manipuler les dates et les heures. Commençons par l'importer dans notre code :

```python
import datetime
```

Maintenant, nous pouvons utiliser la fonction `now()` pour obtenir la date actuelle et l'assigner à une variable :

```python
current_date = datetime.now()
```

Nous pouvons également formater cette date en utilisant la méthode `strftime()` avec un paramètre spécifiant le format souhaité. Par exemple, si nous voulons obtenir la date au format mois/jour/année, nous pouvons utiliser le code suivant :

```python
formatted_date = current_date.strftime("%m/%d/%Y")
```

Et voilà, nous avons maintenant la date actuelle dans le format souhaité dans notre variable `formatted_date`.

Un autre exemple utile serait d'obtenir l'heure actuelle au format 24h :

```python
current_time = datetime.now().strftime("%H:%M:%S")
```

## Plongez Plus Profondément

Maintenant que vous savez comment obtenir la date actuelle avec Python, il peut être intéressant de connaître les différentes méthodes disponibles dans le module `datetime`. Vous pouvez consulter la documentation officielle pour avoir une vue d'ensemble complète de toutes les fonctions disponibles.

De plus, vous pouvez également explorer le module `calendar` de Python qui offre des fonctions pour manipuler les calendriers.

## Voir Aussi

- [Documentation Python du module datetime](https://docs.python.org/fr/3/library/datetime.html)
- [Documentation Python du module calendar](https://docs.python.org/fr/3/library/calendar.html)
- [Article sur l'utilisation du module datetime en Python](https://realpython.com/python-datetime/)
- [Tutoriel pour formater les dates en Python](https://www.freecodecamp.org/news/how-to-format-dates-in-python/)