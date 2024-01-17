---
title:                "Transformer une date en chaîne de caractères"
html_title:           "Bash: Transformer une date en chaîne de caractères"
simple_title:         "Transformer une date en chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?

Convertir une date en une chaîne de caractères, c'est simplement prendre une date écrite sous forme numérique (par exemple 25/09/2021) et la convertir en une chaîne de caractères avec un format pré-défini (par exemple 2021-09-25). Les programmeurs le font souvent pour des raisons de lisibilité, d'affichage ou de stockage de données.

## Comment faire :

``` Bash
# Afficher la date du jour au format AAAA-MM-JJ
date +"%Y-%m-%d"
# Output : 2021-09-25

# Afficher l'année et le mois actuels en lettres
date +"%B %Y"
# Output : September 2021

# Convertir une date en timestamp (nombre de secondes écoulées depuis le 1er janvier 1970)
date -j -f "%d/%m/%Y" "25/09/2021" +%s
# Output : 1632537600

# Afficher la date actuelle en utilisant strftime
# Note : La commande "man strftime" peut être utile pour voir tous les formats disponibles
strftime "%Y/%m/%d" $(date +%s)
# Output : 2021/09/25
```

## Plongée en profondeur :

La conversion d'une date en une chaîne de caractères est un concept assez simple, mais cela peut être un peu plus complexe à mettre en œuvre dans la programmation. En effet, l'utilisation de différents formats et la prise en compte des fuseaux horaires peuvent parfois être nécessaires. Heureusement, Bash offre des options pratiques et flexibles pour effectuer cette tâche, telles que la commande `date` et les options `+%x` et `+%X`.

Il existe également d'autres langages de programmation tels que Python, PHP ou Java qui ont leurs propres méthodes pour convertir une date en une chaîne de caractères. À vous de trouver celui qui convient le mieux à votre projet !

## Voir aussi :

- La documentation officielle de `date` pour en savoir plus sur les options de format : https://www.unix.com/man-page/mojave/1/date/
- Un tutoriel sur la conversion de dates en chaînes de caractères en Python : https://realpython.com/python-datetime/
- Une explication détaillée sur la conversion de dates en PHP : https://www.w3schools.in/php-programming/datetime/