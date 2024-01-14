---
title:    "Python: Comparaison de deux dates"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi

La comparaison de deux dates est une tâche commune dans la programmation, qui peut être utile dans de nombreuses situations. Par exemple, il peut être nécessaire de vérifier si une date est antérieure ou postérieure à une autre, ou encore de calculer la différence entre deux dates. Dans cet article, nous allons découvrir comment comparer deux dates en utilisant Python.

# Comment faire

Pour comparer deux dates en Python, nous pouvons utiliser le module intégré "datetime". Tout d'abord, nous devons importer ce module en utilisant la commande suivante :

```Python
import datetime
```

Ensuite, nous créons deux objets "datetime" en utilisant la fonction "datetime()" et en spécifiant les dates que nous voulons comparer. Par exemple :

```Python
first_date = datetime.datetime(2020, 5, 14)
second_date = datetime.datetime(2021, 2, 17)
```

Maintenant, nous pouvons comparer ces deux dates en utilisant les opérateurs de comparaison standard de Python, tels que "==", "!=", "<" et ">". Par exemple :

```Python
# Comparaison avec ==
if first_date == second_date:
    print("Les dates sont identiques.")
else:
    print("Les dates sont différentes.")

# Comparaison avec <
if first_date < second_date:
    print("La première date est antérieure à la seconde.")
else:
    print("La première date est postérieure à la seconde.")
```

La sortie de cet exemple serait :

```
Les dates sont différentes.
La première date est antérieure à la seconde.
```

# Deep Dive

Il est important de noter que les dates doivent être au format "datetime" pour être comparées. Si vous avez des dates sous forme de chaînes de caractères, vous devrez d'abord les convertir en objets "datetime" en utilisant la fonction "strptime()". De plus, le module "datetime" possède également des méthodes pratiques telles que "date()" ou "time()" pour extraire uniquement la date ou l'heure d'un objet "datetime". Pour plus d'informations, vous pouvez consulter la documentation officielle de Python sur le module "datetime".

# Voir aussi

- Documentation officielle de Python sur le module "datetime" : https://docs.python.org/fr/3.7/library/datetime.html
- Tutoriel sur la manipulation des dates en Python : https://www.learnpython.org/fr/Dates
- Vidéo explicative sur la comparaison de dates en Python : https://www.youtube.com/watch?v=Eyg4XDIGR0k