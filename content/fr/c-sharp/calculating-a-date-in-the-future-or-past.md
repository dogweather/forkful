---
title:                "C#: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de calculer une date dans le futur ou dans le passé dans le cadre de la programmation en C#. Que ce soit pour planifier des événements, effectuer des calculs temporels ou tout autre scénario, il est important de comprendre comment le faire correctement. Dans cet article, nous allons explorer les différentes façons de calculer une date dans le futur ou dans le passé en utilisant le langage C#.

## Comment faire

La première étape pour calculer une date dans le futur ou dans le passé est de choisir une méthode pour le faire. Voici quelques exemples de méthodes couramment utilisées en C#:

```C#
// Calculer une date dans le futur en utilisant DateTime.AddYears()
DateTime dateDansLeFutur = DateTime.Now.AddYears(2);
Console.WriteLine(dateDansLeFutur);
// Sortie: 09/07/2022 14:30:15

// Calculer une date dans le passé en soustrayant des jours de DateTime.Today
DateTime dateDansLePasse = DateTime.Today.AddDays(-30);
Console.WriteLine(dateDansLePasse);
// Sortie: 09/06/2021 00:00:00

// Utiliser la classe TimeSpan pour ajouter ou soustraire une période de temps à une date donnée
TimeSpan periode = new TimeSpan(2, 0, 0, 0); // 2 jours
DateTime nouvelleDate = DateTime.Now + periode; // Ajouter 2 jours à la date actuelle
Console.WriteLine(nouvelleDate);
// Sortie: 11/07/2021 14:30:15
```

Il est également important de prendre en compte les dates bissextiles lors du calcul de dates dans le futur ou dans le passé. Cela peut être fait en utilisant la méthode DateTime.IsLeapYear() pour vérifier si une année est bissextile ou non.

## Plongée en profondeur

Lors du calcul de dates dans le futur ou dans le passé, il est important de comprendre les différentes nuances de chaque méthode utilisée. Par exemple, lors de l'utilisation de DateTime.AddYears(), la date résultante sera calculée en tenant compte des années bissextiles. Cependant, lors de la soustraction de jours de DateTime.Today, les années bissextiles ne seront pas prises en compte, ce qui peut entraîner des résultats incorrects.

Une autre chose à prendre en compte est la manipulation des zones horaires et des changements d'heure lors du calcul de dates dans le futur ou dans le passé. Il est recommandé d'utiliser les méthodes de la classe TimeZone pour gérer ces cas.

## Voir aussi

- [Microsoft Documentation on DateTime structure](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [C# Date and Time tutorial by Tutorialspoint](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- [DateTime class in C# by GeeksforGeeks](https://www.geeksforgeeks.org/datetime-class-in-c-sharp/)