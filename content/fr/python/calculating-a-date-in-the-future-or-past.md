---
title:    "Python: Calculer une date dans le futur ou le passé"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

La programmation Python offre de nombreuses fonctionnalités utiles pour la gestion et la manipulation de dates. Calculer une date dans le futur ou dans le passé peut être particulièrement pratique pour planifier des événements, suivre des délais ou analyser des données temporelles. Dans cet article, nous allons explorer comment effectuer ces calculs en utilisant Python.

## Comment faire

Tout d'abord, nous devons importer le module "datetime" qui nous permettra de travailler avec des dates dans Python. Ensuite, nous pouvons utiliser la méthode "date" pour définir une date spécifique et stocker cette valeur dans une variable.

```Python
import datetime

ma_date = datetime.date(2020, 5, 20)
```

Maintenant, si nous voulons calculer une date dans le futur à partir de cette date, nous pouvons utiliser la méthode "timedelta" et indiquer le nombre de jours à ajouter. Nous pouvons également utiliser des opérations mathématiques pour calculer une date dans le passé en ajoutant ou en soustrayant des jours à notre date initiale.

```Python
# Calculer une date dans le futur (ajouter 5 jours)
date_futur = ma_date + datetime.timedelta(days=5)

# Calculer une date dans le passé (soustraire 10 jours)
date_passé = ma_date - datetime.timedelta(days=10)

# Afficher les résultats
print(date_futur) # 2020-05-25
print(date_passé) # 2020-05-10
```

Nous pouvons également utiliser les méthodes "year", "month" et "day" pour accéder aux valeurs individuelles d'une date et les utiliser pour calculer une nouvelle date.

```Python
# Accéder aux valeurs d'une date
print(ma_date.year) # 2020
print(ma_date.month) # 5
print(ma_date.day) # 20

# Calculer une date dans le futur
nouvelle_date = datetime.date(ma_date.year + 1, ma_date.month, ma_date.day)

# Afficher le résultat
print(nouvelle_date) # 2021-05-20
```

## Plongée en profondeur

Python offre une grande flexibilité pour manipuler des dates en utilisant différents formats et méthodes. Par exemple, nous pouvons utiliser la méthode "strftime" pour formater une date selon un modèle spécifique.

```Python
# Formater une date au format jour/mois/année
print(ma_date.strftime("%d/%m/%Y")) # 20/05/2020

# Formater une date au format complet
print(ma_date.strftime("%A, %d %B %Y")) # Wednesday, 20 May 2020
```

Il est également possible de calculer des dates non seulement à partir d'une date initiale, mais aussi à partir d'une chaîne de caractères représentant une date. Pour ce faire, nous pouvons utiliser la méthode "strptime" pour convertir la chaîne en un objet de type "date".

```Python
# Convertir une chaîne en date
autre_date = datetime.datetime.strptime("12/04/2018", "%d/%m/%Y")

# Calculer une date dans le futur à partir de cette chaîne
date_futur2 = autre_date + datetime.timedelta(weeks=10)

# Afficher le résultat
print(date_futur2) # 2018-06-21
```

Enfin, il est important de noter que Python offre également des fonctionnalités pour gérer les dates avec des fuseaux horaires et des données relatives telles que les années bissextiles.

## Voir aussi

Vous pouvez consulter la documentation officielle de Python pour en savoir plus sur la manipulation de dates en Python : [Documentation officielle de Python](https://docs.python.org/fr/3/library/datetime.html)

Vous pouvez également consulter nos articles sur les opérations mathématiques en Python et sur la manipulation des chaînes de caractères pour en savoir plus sur les concepts utilisés dans cet article.