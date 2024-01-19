---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "C#: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calculer une date dans le futur ou le passé en C#

## Qu'est-ce et pourquoi?

Calculer une date dans le futur ou le passé signifie définir une date précise en avant ou en arrière à partir de la date actuelle. Les programmeurs en ont besoin pour gérer efficacement le temps dans les applications, comme la programmation d'événements ou le suivi des délais.

## Comment faire

Voici comment calculer une date dans le futur et dans le passé avec C# :

```C#
// Obtention de la date d'aujourd'hui
DateTime today = DateTime.Now;

// Calcul d'une date dans le futur
DateTime futureDate = today.AddDays(30);
Console.WriteLine(futureDate);

// Calcul d'une date dans le passé
DateTime pastDate = today.AddDays(-30);
Console.WriteLine(pastDate);
```
Exemple de sortie :

```
2022-03-15 13:14:22
2022-01-15 13:14:22
```

## Examen approfondi

Le calcul des dates dans le futur ou le passé est une pratique courante depuis les débuts de l'informatique. Pour un contexte historique, l'horloge interne des ordinateurs a été conçue pour compter les secondes passées depuis une date spécifique, à savoir le 1er janvier 1970. Cette date est communément appelée l' "epoch Unix".

En ce qui concerne les alternatives, vous pouvez utiliser également Noda Time - une bibliothèque pour le calcul du temps en C#. Elle offre plus de flexibilité et de précision que les méthodes standard de DateTime en C#.

Pour ce qui est des détails de mise en œuvre, C# rend le calcul d'une date dans le futur ou le passé plutôt simple grâce à ses méthodes intégrées. Par exemple, `AddDays` est une méthode de la classe `DateTime`, qui retourne une nouvelle instance de `DateTime` après l'ajout du nombre spécifié de jours à l'instance `DateTime`.

## Pour en savoir plus

- Documentation .NET sur DateTime: [https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime)
- Bibliothèque Noda Time : [https://nodatime.org/](https://nodatime.org/)
- "Epoch Unix" Wikipédia : [https://fr.wikipedia.org/wiki/Heure_Unix](https://fr.wikipedia.org/wiki/Heure_Unix)