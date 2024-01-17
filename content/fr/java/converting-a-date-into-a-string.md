---
title:                "Converting une date en une chaîne de caractères"
html_title:           "Java: Converting une date en une chaîne de caractères"
simple_title:         "Converting une date en une chaîne de caractères"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
La conversion d'une date en une chaîne de caractères est un concept commun en programmation Java. Cela implique de transformer une date dans un format utilisable par l'ordinateur en une chaîne de caractères qui peut être comprise et affichée par les utilisateurs. Les programmeurs le font pour faciliter la visualisation des dates dans un format familier pour les utilisateurs.

## Comment faire:
````Java
// Exemple 1: Conversion d'une date en chaîne de caractères dans le format "dd/mm/yyyy"
SimpleDateFormat formatter = new SimpleDateFormat("dd/mm/yyyy");
Date date = new Date();
String strDate = formatter.format(date);
System.out.println(strDate); // Résultat: "05/10/2021"

// Exemple 2: Conversion d'une date en chaîne de caractères dans le format "jour/mois/année"
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("EEEE/MMMM/yyyy", Locale.FRENCH);
LocalDate date = LocalDate.now();
String strDate = date.format(formatter);
System.out.println(strDate); // Résultat: "mardi/octobre/2021"
````

## Profonde plongée:
La conversion de dates en chaînes de caractères est un problème récurrent depuis les débuts de la programmation informatique. Avant l'avènement des ordinateurs, les êtres humains utilisaient des systèmes complexes pour représenter les dates, tels que les calendriers basés sur la lune et les années bissextiles. Avec l'utilisation croissante des ordinateurs, il est devenu nécessaire de normaliser la représentation des dates pour faciliter leur traitement. Alternativement, certaines langues de programmation offrent des méthodes intégrées pour gérer les dates, mais elles peuvent être plus complexes à utiliser que la conversion en chaîne de caractères.

## Voir aussi:
- [Documentation officielle de Java sur la classe SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Documentation officielle de Java sur la classe DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Tutoriel sur la conversion de dates en chaînes de caractères en Java](https://www.baeldung.com/java-date-to-string)