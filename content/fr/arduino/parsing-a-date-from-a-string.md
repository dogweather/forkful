---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Arduino: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Le parsing de date à partir d'une chaîne de caractères est tout simplement le fait de convertir une date sous forme de texte en une date reconnaissable par un ordinateur. Les programmeurs le font couramment pour traiter des données ou pour afficher des informations temporelles dans un format compréhensible pour les utilisateurs. 

## Comment faire:

Voici un exemple de code Arduino pour parser une date à partir d'une chaîne de caractères "18/06/2021" :

```Arduino
// Déclaration d'une variable de type String contenant la date
String dateString = "18/06/2021";

// Conversion en un objet de type Date
Date date = parseDate(dateString);
 
// Affichage de la date au format jour/mois/année
Serial.println(date.day() + "/" + date.month() + "/" + date.year());
```

La sortie de ce code sera "18/06/2021".

## Exploration en profondeur:

Le parsing de date à partir d'une chaîne de caractères est une opération courante dans la programmation, et est souvent utilisé pour traiter des données dans les applications web ou pour afficher des horaires dans des applications mobiles.

Les alternatives au parsing de date incluent l'utilisation de bibliothèques tierces qui peuvent traiter les dates de manière plus efficace et flexible. 

Dans Arduino, le type de données "String" est souvent utilisé pour manipuler les chaînes de caractères, mais il est important de faire attention à la gestion de la mémoire pour éviter les fuites de mémoire qui peuvent ralentir le fonctionnement de votre programme.

## À voir également:

Pour en savoir plus sur le parsing de date en Arduino, vous pouvez consulter la documentation officielle d'Arduino et découvrir d'autres façons d'exploiter cette fonctionnalité dans vos projets. Vous pouvez également consulter des tutoriels en ligne pour apprendre à gérer les chaînes de caractères de manière efficace dans vos codes Arduino.