---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est ce & Pourquoi?

Obtenir la date courante en C# signifie trouver la date et l'heure exactes au moment où le code s'exécute. C'est souvent utilisé pour les journaux de bord (log files), les horodatages, et les mesures de performance.

## Comment faire :

Voici comment obtenir la date courante en C# :
```
C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```
L'output peut s'afficher sous cette forme : `2022-10-11 18:10:24.456789`

On peut aussi afficher uniquement la date:
```
C#
DateTime currentDate = DateTime.Today;
Console.WriteLine(currentDate);
```
L'output s'affichera sous cette forme : `2022-10-11 00:00:00`

## Plongée en profondeur

Historiquement, `DateTime.Now` est utilisée depuis l'introduction de .NET. Plus récemment, nous avons obtenu une alternative plus précise en termes de millisecondes, `DateTimeOffset.Now`.

Pour obtenir uniquement la date sans indication de temps, on utilise `DateTime.Today` ou `DateTimeOffset.Today`.

Si vous avez besoin d'une horloge haute-résolution pour la mesure de performance, envisagez d'utiliser `Stopwatch` au lieu de `DateTime`.

## Pour aller plus loin

Visitez ces liens pour une information plus détaillée:
- [La documentation officielle du Microsoft C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-6.0)
- [StackOverflow : DateTime vs DateTimeOffset](https://stackoverflow.com/questions/4331189/datetime-vs-datetimeoffset)
- [Discussion: Mesurer le temps en C#](https://www.c-sharpcorner.com/UploadFile/BlogAuthor/942-improve-the-performance-of-a-c-sharp-application/)