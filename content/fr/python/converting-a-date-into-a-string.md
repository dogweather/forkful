---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Python: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi?
Convertissez une date en une chaîne de caractères est une tâche courante en programmation. Cela signifie simplement prendre une date au format de données et la transformer en une représentation textuelle.

Les programmeurs font cela pour pouvoir afficher les dates de manière lisible pour les utilisateurs, ainsi que pour faciliter la comparaison et la manipulation de dates dans le code.

# Comment faire:
Les exemples suivants montrent comment convertir une date en une chaîne de caractères en utilisant la librairie datetime en Python:

```Python
from datetime import datetime

# Convertir une date en une chaîne de caractères avec le format par défaut
date = datetime(2020, 10, 15)
str_date = date.strftime("%d-%m-%Y")
print(str_date) # output: 15-10-2020

# Changer le format en français
str_date = date.strftime("%d %B %Y")
print(str_date) # output: 15 octobre 2020
```

# Plongée profonde:
Historiquement, les dates étaient stockées sous forme de nombres dans les programmes, rendant la manipulation et l'affichage difficile. La librairie datetime en Python a été introduite pour simplifier le travail avec les dates en fournissant des méthodes pour convertir les dates en chaînes de caractères.

Il existe également d'autres façons de représenter les dates en texte, telles que le format ISO 8601 qui suit une structure année/mois/jour et permet une meilleure lisibilité pour les machines.

L'implémentation de la conversion d'une date en une chaîne de caractères peut varier en fonction du langage de programmation utilisé. Par exemple, en JavaScript, on peut utiliser la fonction ```toDateString()``` pour obtenir une version courte de la date.

# Voir aussi:
Pour plus d'informations sur la librairie datetime en Python, vous pouvez consulter la documentation officielle: https://docs.python.org/fr/3/library/datetime.html

Pour en savoir plus sur les normes et conventions de représentation des dates en programmation, vous pouvez consulter cet article: https://www.clivern.com/format-dates-python/

Et voici un guide pratique sur la manipulation et la conversion de dates dans différents langages de programmation: https://www.dateformat.io/