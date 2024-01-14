---
title:    "Python: Convertir une date en une chaîne de caractères"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous travaillez avec des dates dans vos programmes Python, il peut être utile de les convertir en chaînes de caractères. Cela peut être pour des raisons d'affichage, pour faciliter la comparaison entre les dates ou pour les stocker dans un fichier texte. Dans cet article, nous allons explorer comment convertir une date en chaîne de caractères en utilisant Python.

## Comment faire

Tout d'abord, nous avons besoin d'importer le module de date et de temps de Python :

```Python
import datetime
```

Ensuite, nous pouvons créer un objet de type date en utilisant la fonction "date" de ce module :

```Python
today = datetime.date(2021, 9, 30)
```

Ce code crée un objet "today" avec la date du 30 septembre 2021. Maintenant, pour convertir cette date en chaîne de caractères, nous pouvons utiliser la méthode "strftime" (f pour "format") de cet objet :

```Python
s_today = today.strftime('%d/%m/%Y')
```

Ici, nous utilisons le format "JJ/MM/AAAA" pour la chaîne de caractères, mais vous pouvez utiliser n'importe quel format que vous souhaitez. Maintenant, si nous imprimons cette chaîne de caractères, nous aurons :

```Python
print(s_today)
```
Output : "30/09/2021"

Pour aller plus loin, nous pouvons également ajouter des informations supplémentaires à notre chaîne de caractères, telles que le jour de la semaine ou le mois en lettres :

```Python
s_today_full = today.strftime('%A, %d %B %Y')
```

Output : "jeudi, 30 septembre 2021"

## Plongée en profondeur

La méthode "strftime" prend différents arguments pour créer une chaîne de caractères avec différents formats. Voici quelques exemples de formats que vous pouvez utiliser :

- %d : le jour du mois (avec zéro devant si besoin)
- %m : le mois (avec zéro devant si besoin)
- %Y : l'année sur 4 chiffres
- %y : l'année sur 2 chiffres
- %A : le jour de la semaine en lettres
- %a : le jour de la semaine en abrégé
- %B : le mois en lettres
- %b : le mois en abrégé

Vous pouvez également utiliser des caractères spéciaux pour séparer les éléments de la date, tels que "/", "-", "." :

```Python
s_today_full = today.strftime('%d/%m/%y')
```

Output : "30/09/21"

N'hésitez pas à explorer différents formats pour trouver celui qui convient le mieux à vos besoins.

## Voir aussi

- [Documentation officielle de Python sur le module datetime](https://docs.python.org/fr/3/library/datetime.html)
- [Tutoriel YouTube sur la conversion de date en chaîne de caractères en Python (en français)](https://www.youtube.com/watch?v=L7LjU9tBjio)
- [Article sur la manipulation de dates en Python (en français)](https://blog.paumard.org/cours/python3/chapitre07-datetime.html)