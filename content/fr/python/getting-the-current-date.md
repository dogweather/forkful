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

## Qu'est-ce que c'est et pourquoi?
Obtenir la date actuelle est un moyen courant pour les programmeurs de garder une trace du temps dans leurs programmes. Cela peut être utile pour enregistrer quand une tâche a été effectuée ou pour personnaliser les résultats en fonction de la date actuelle.

## Comment faire:
Voici deux façons simples d'obtenir la date actuelle en utilisant Python:

```
# Importer le module datetime 
import datetime 

# Utiliser la fonction maintenant () pour obtenir la date et l'heure actuelles 
maintenant = datetime.now() 

# Afficher la date actuelle dans le format "Mois / Jour / Année, Heure: Minutes: Secondes" 
print(maintenant.strftime("%m/%d/%Y, %H:%M:%S")) 

# Output: 04/14/2021, 10:23:09 
```

```
# Importer le module time 
import time 

# Utiliser la fonction time () pour obtenir le nombre de secondes écoulées depuis le 01/01/1970 
seconde = time.time() 

# Utiliser la fonction localtime () pour convertir le nombre de secondes en une structure de temps locale 
struct_time = time.localtime(seconde) 

# Afficher la date actuelle dans le format "Mois / Jour / Année, Heure: Minutes: Secondes" 
print(time.strftime("%m/%d/%Y, %H:%M:%S", struct_time)) 

# Output: 04/14/2021, 10:23:09 
```

## Plongée en profondeur:
Obtenir la date actuelle peut sembler simple, mais c'était en fait un défi pour les programmeurs dans les premiers jours de la programmation informatique. Les programmes doivent souvent être en mesure de gérer différentes fuseaux horaires, les changements d'heure et les années bissextiles. Heureusement, les modules datetime et time de Python s'occupent désormais de tous ces problèmes.

Il existe également d'autres alternatives pour obtenir la date actuelle, telles que les modules third party tels que arrow et pendulum. Ces modules offrent des fonctionnalités supplémentaires comme la conversion entre différents fuseaux horaires et la manipulation des dates avec plus de précision.

Si vous souhaitez en savoir plus sur l'implémentation de la date actuelle dans Python, vous pouvez consulter la documentation officielle de la ligne de commande strftime ou le livre "Dive Into Python" de Mark Pilgrim.

## Voir aussi:
- [Documentation officielle strftime] (https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)
- [Dive Into Python] (https://diveintopython3.net/datetime.html)