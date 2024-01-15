---
title:                "Obtenir la date actuelle"
html_title:           "Python: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes nouveau dans le monde de la programmation, il est probable que vous ayez entendu parler de l'importance de savoir comment obtenir la date actuelle en Python. Mais pourquoi est-ce si important ? Eh bien, la réponse est simple : la plupart des applications et des programmes informatiques nécessitent l'utilisation de dates et d'heures pour gérer des données, suivre les événements et bien plus encore. Comme Python est l'un des langages de programmation les plus populaires et polyvalents, il est utile de comprendre comment obtenir la date actuelle en utilisant ce langage.

## Comment faire
Il existe plusieurs façons d'obtenir la date actuelle en Python, mais nous allons en aborder deux : en utilisant le module "datetime" et en utilisant la bibliothèque "time".

### Utiliser le module "datetime"
Pour utiliser le module "datetime", vous devez d'abord l'importer dans votre code en utilisant la commande suivante :
```Python
import datetime
```
Ensuite, vous pouvez utiliser la méthode "now()" pour obtenir la date et l'heure actuelles, comme illustré dans l'exemple ci-dessous :
```Python
import datetime

now = datetime.now()

print(now)
```
Cela produira un résultat similaire à ceci : 
```Python
2021-10-19 15:08:38.454442
```

Vous pouvez également formater la sortie pour n'afficher que la date ou l'heure, en utilisant les attributs "date()" et "time()" respectivement. Par exemple :
```Python
import datetime

now = datetime.now()

print(now.date())
print(now.time())
```
Ce code produira une sortie similaire à ceci :
```Python
2021-10-19
15:09:46.472323
```

### Utiliser la bibliothèque "time"
La bibliothèque "time" offre également des fonctions pour obtenir la date et l'heure actuelles en Python. Vous pouvez l'importer dans votre code en utilisant la commande suivante :
```Python
import time
```
Ensuite, vous pouvez utiliser la fonction "time()" pour obtenir le temps écoulé depuis le 1er janvier 1970 à minuit, comme illustré dans l'exemple ci-dessous :
```Python
import time

current_time = time.time()

print(current_time)
```
Ce code produira un résultat similaire à ceci :
```Python
1634668697.3228226
```

Si vous souhaitez obtenir une sortie plus lisible, vous pouvez utiliser la fonction "localtime()" pour la formater en une structure de temps plus compréhensible, comme ceci :
```Python
import time

current_time = time.time()

local_time = time.localtime(current_time)

print(local_time)
```
Et voici un exemple de la sortie produite :
```Python
time.struct_time(tm_year=2021, tm_mon=10, tm_mday=19, tm_hour=15, tm_min=13, tm_sec=48, tm_wday=1, tm_yday=292, tm_isdst=0)
```

## Deep Dive
Maintenant que nous avons vu comment obtenir la date actuelle en utilisant Python, il est important de comprendre que la date et l'heure sont stockées dans un format spécifique à Python. Pour être exact, elles sont stockées sous forme d'objet "datetime" ou "time", respectivement avec le module "datetime" et la bibliothèque "time".

Un élément important à noter est que ces objets "datetime" et "time" sont considérés comme des objets immuables, ce qui signifie qu'ils ne peuvent pas être modifiés après leur création. Cela peut sembler frustrant, mais cela garantit que vous ne modifiez pas accidentellement les données de date et d'heure dans vos programmes.

De plus, il est important de comprendre que la date et l'heure utilisent différentes unités : la date utilise des unités telles que l'année, le mois et le jour, tandis que l'heure utilise des unités telles que l'heure, la minute et la seconde. En gardant cela à l'esprit, vous pourrez utiliser les méthodes appropriées pour récupérer l'information dont vous avez besoin.

## Voir aussi
Maintenant que vous avez appris comment obtenir la date actuelle en Python, vous pouvez essayer d'autres manipulations de date et d'heure en utilisant