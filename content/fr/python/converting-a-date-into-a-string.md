---
title:                "Python: Convertir une date en chaîne de caractères"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi Convertir une Date en Chaîne ?

De nombreuses fois, en tant que programmeur, vous serez confronté à la question de comment afficher une date de manière lisible pour l'utilisateur. Python fournit une méthode simple pour ce faire, en convertissant une date en une chaîne de caractères.

## Comment Faire

Dans Python, la fonction "strptime()" est utilisée pour convertir une date en objet "datetime" tandis que la fonction "strftime()" est utilisée pour la convertir en chaîne de caractères. Voyons un exemple de code :

```Python
from datetime import datetime

# Création d'un objet datetime avec la date du 20 avril 2021
date = datetime(2021, 4, 20)

# Convertir la date en chaîne de caractères avec le format "dd/mm/yyyy"
str_date = date.strftime("%d/%m/%Y")

print(str_date) # 20/04/2021
```

La fonction "strftime()" utilise des lettres pour spécifier le format de la chaîne de caractères. Voici quelques-unes des lettres les plus utilisées :

- %d : Jour du mois sous forme numérique (ex : 01, 02, 03...)
- %m : Mois sous forme numérique (ex : 01, 02, 03...)
- %Y : Année sur 4 chiffres (ex : 2021, 2022, 2023...)
- %H : Heure au format 24 heures (ex : 13, 14, 15...)
- %M : Minutes (ex : 01, 02, 03...)

Vous pouvez utiliser ces lettres dans n'importe quel ordre pour créer un format de chaîne de caractères personnalisé.

## Plongée Profonde

La méthode de conversion en chaîne de caractères peut également être utilisée pour afficher la date et l'heure actuelles. Voyons un exemple :

```Python
from datetime import datetime

# Obtenez la date et l'heure actuelles
now = datetime.now()

# Convertir en chaîne de caractères avec le format "dd/mm/yyyy - hh:mm"
str_now = now.strftime("%d/%m/%Y - %H:%M")

print(str_now) # 13/10/2021 - 10:45
```

La méthode "strptime()" peut également être utilisée pour convertir une chaîne de caractères en objet "datetime". Cela peut être utile si vous avez besoin de convertir une date entrée par l'utilisateur en objet "datetime" pour effectuer des opérations telles que la comparaison de dates.

## Voir Aussi

- Documentation officielle de Python sur la classe "datetime" : https://docs.python.org/fr/3/library/datetime.html
- Tutoriel sur le formatage de dates en Python : https://www.geeksforgeeks.org/python-formatting-dates-in-python/
- Vidéo YouTube sur la manipulation de dates en Python : https://www.youtube.com/watch?v=eirjjyP2qcQ

Merci d'avoir lu cet article sur la conversion de dates en chaînes en Python. J'espère que cela vous sera utile dans vos projets futurs !