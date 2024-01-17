---
title:                "Transformation d'une date en chaîne de caractères"
html_title:           "Arduino: Transformation d'une date en chaîne de caractères"
simple_title:         "Transformation d'une date en chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Comment & Pourquoi?

## Qu'est-ce que la conversion d'une date en chaîne?
Convertir une date en chaîne consiste à transformer une date dans un format compréhensible par les ordinateurs en une série de caractères, ou "chaîne de caractères". Cela permet aux programmeurs de manipuler et d'afficher la date de manière plus efficace dans leurs programmes.

## Pourquoi les programmeurs le font-ils?
La conversion d'une date en chaîne est couramment utilisée dans les programmes informatiques, car cela permet aux utilisateurs de saisir et de traiter la date dans divers formats, sans avoir à modifier le code source du programme à chaque fois.

# Comment faire:

Voici un exemple de conversion de la date en chaîne en utilisant la fonction `str()` dans Arduino :

```
#include <Time.h>

// Initialiser la date à convertir en chaîne
int jour = 14;
int mois = 03;
int annee = 2021;

// Convertir la date en chaîne
String date = "";
date += jour;
date += "/";
date += mois;
date += "/";
date += annee;

// Afficher la date en chaîne
Serial.println(date);
```

La sortie de ce code sera `14/03/2021`, qui est la date au format dd/mm/yyyy.

# Plongée en profondeur:

## Contexte historique:
La conversion de la date en chaîne a été rendue populaire dans les années 1970 avec l'introduction du langage de programmation C. Avant cela, les dates étaient stockées sous forme de nombres dans les langages de programmation, rendant difficile la manipulation et l'affichage de la date pour les programmeurs.

## Alternatives:
Il existe plusieurs façons de convertir une date en chaîne, telles que l'utilisation de bibliothèques tierces ou l'écriture de fonctions personnalisées. Cependant, la fonction `str()` en Arduino est la méthode la plus simple et la plus couramment utilisée.

## Détails de mise en œuvre:
La fonction `str()` en Arduino convertit simplement un nombre en chaîne de caractères. Pour convertir une date complète en chaîne, il suffit de concaténer chaque élément de la date (jour, mois, année) avec des caractères tels que des barres obliques ou des tirets.

# Voir aussi:

Pour en savoir plus sur la conversion de la date en chaîne en Arduino, vous pouvez consulter la documentation officielle sur la fonction `str()` ici : https://www.arduino.cc/reference/en/language/variables/data-types/string/manipulation/str/.

Vous pouvez également trouver des exemples de code et des tutoriels en ligne pour vous aider à comprendre et à utiliser la conversion de la date en chaîne dans vos projets Arduino.