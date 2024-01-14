---
title:    "Python: Convertir une date en chaîne de caractères"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire dans les projets de programmation de convertir une date en chaîne de caractères, par exemple pour l'afficher dans un format spécifique ou pour l'utiliser comme clé dans une base de données. Dans cet article, nous allons explorer comment le faire en utilisant Python.

## Comment faire

Pour convertir une date en chaîne de caractères en Python, nous allons utiliser le module `datetime` et la méthode `strftime()`. Tout d'abord, importons le module :

```python
import datetime
```

Ensuite, créons un objet `datetime` avec la date que nous voulons convertir :

```python
my_date = datetime.date(2021, 9, 10)
```

Enfin, utilisons la méthode `strftime()` pour spécifier le format de la chaîne de caractères souhaité et l'appliquer à notre objet `datetime` :

```python
my_string_date = my_date.strftime("%d/%m/%Y")
print(my_string_date)
```

La sortie de code sera : `10/09/2021`.

## Deep Dive

La méthode `strftime()` prend en argument une chaîne de caractères pour définir le format souhaité. Voici quelques spécificateurs de format couramment utilisés :

- `%Y` : année complète (ex. 2021)
- `%m` : mois en chiffres (ex. 09)
- `%d` : jour en chiffres (ex. 10)
- `%B` : mois complet en lettres (ex. Septembre)
- `%A` : jour complet en lettres (ex. Vendredi)
- `%H` : heure au format 24 heures (ex. 13)
- `%I` : heure au format 12 heures (ex. 01)
- `%M` : minutes en chiffres (ex. 30)
- `%S` : secondes en chiffres (ex. 45)

Il est également possible d'ajouter des caractères spéciaux entre les spécificateurs de format pour personnaliser la chaîne de caractères de sortie. Par exemple, `%B %d, %Y` donnera `Septembre 10, 2021`.

Pour une liste complète des spécificateurs de format et de leurs résultats, consultez la documentation officielle de Python.

## Voir aussi

Pour plus d'informations sur la manipulation de dates en Python, consultez les liens suivants :

- [Documentation officielle de Python sur le module datetime](https://docs.python.org/fr/3/library/datetime.html)
- [Article "Manipulating Dates and Time in Python" de Real Python (en anglais)](https://realpython.com/python-datetime/)
- [Vidéo "Les Dates en Python" de Codecademy (en français)](https://www.youtube.com/watch?v=RkRTkMkRuM4)