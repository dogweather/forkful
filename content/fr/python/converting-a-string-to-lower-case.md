---
title:                "Convertir une chaîne en minuscules"
html_title:           "PHP: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Convertir une chaîne de caractères en minuscules consiste à transformer toutes les lettres majuscules en lettres minuscules. Les programmeurs le font souvent pour comparer les chaînes de manière insensible à la casse ou pour normaliser les données.

## Comment faire :
Python offre une méthode simple pour convertir une chaîne en minuscules : `lower()`. Voyez comment cela fonctionne en pratique :
```Python
ma_chaine = "Bonjour le Monde"
ma_chaine_en_minuscules = ma_chaine.lower()

print(ma_chaine_en_minuscules)
# sortie : bonjour le monde
```
Il n'y a pas de paramètres pour `lower()`, et la méthode retourne une nouvelle chaîne où toutes les lettres majuscules ont été converties en minuscules.

## Deep Dive
La fonction `lower()` existe depuis les débuts de Python et reste la manière la plus courante de convertir une chaîne en minuscules. Cependant, d'autres approches pourraient être utilisées dans des cas plus spécifiques. Les alternatives incluent les expressions régulières et les compréhensions de liste.

Sous le capot, `lower()` fonctionne en mappant chaque point de code Unicode vers son équivalent en minuscules selon les règles du standard Unicode. Ceci signifie que `lower()` fonctionne avec n'importe quelle langue ou symbole supporté par Unicode, et pas seulement l'anglais.

## Pour aller plus loin
Pour approfondir votre compréhension du sujet, jetez un oeil à ces ressources supplémentaires :
1. Documentation officielle Python sur les méthodes de chaîne : https://docs.python.org/fr/3/library/stdtypes.html#str.lower
2. Article sur le standard Unicode : https://fr.wikipedia.org/wiki/Unicode
3. Guide pratique sur les expressions régulières en Python : https://docs.python.org/fr/3/howto/regex.html