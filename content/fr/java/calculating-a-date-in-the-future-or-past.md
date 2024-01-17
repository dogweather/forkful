---
title:                "Calcul d'une date dans le futur ou le passé."
html_title:           "Java: Calcul d'une date dans le futur ou le passé."
simple_title:         "Calcul d'une date dans le futur ou le passé."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi?
Calculer une date dans le futur ou dans le passé est l'action de modifier une date existante en ajoutant ou en soustrayant un certain nombre de jours, mois ou années. Les programmeurs le font souvent pour automatiser des tâches telles que la gestion des rendez-vous, les rappels d'événements ou la génération de rapports sur une période spécifique.

## Comment faire:
Pour calculer une date dans le futur ou dans le passé en utilisant Java, il existe plusieurs méthodes disponibles dans la classe ```java.util.Calendar```. Par exemple, pour ajouter 7 jours à la date actuelle et l'afficher au format jour/mois/année, le code suivant peut être utilisé:
```
Calendar calendar = Calendar.getInstance();
calendar.add(Calendar.DAY_OF_YEAR, 7);
System.out.println(calendar.get(Calendar.DAY_OF_MONTH) + "/" + (calendar.get(Calendar.MONTH)+1) + "/" + calendar.get(Calendar.YEAR));
```
La sortie serait 03/04/2021 si exécuté le 27 mars 2021.

## Approfondissement:
L'utilisation de la classe Calendar pour calculer des dates dans le futur ou dans le passé remonte à la version 1.1 de Java et est toujours largement utilisée malgré la sortie de la classe LocalDate dans Java 8. Une autre méthode courante est d'utiliser la bibliothèque Joda-Time, qui fournit des fonctionnalités plus avancées pour manipuler les dates et les heures dans Java.

## Voir aussi:
- [Documentation officielle Java 8 sur la manipulation de dates et heures](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tutoriel sur l'utilisation de la classe Calendar pour calculer des dates dans le futur ou dans le passé](https://www.baeldung.com/java-date-calculation)
- [Tutoriel sur l'utilisation de la bibliothèque Joda-Time pour manipuler les dates et heures en Java](https://www.joda.org/joda-time/userguide.html)