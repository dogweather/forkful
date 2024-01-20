---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Le fractionnement d'une date à partir d'une chaîne de caractères est une pratique courante en programmation consistant à convertir une date sous forme de texte en une structure de date C#. C'est essentiel pour manipuler et analyser les données de date de manière plus efficace.

## Comment faire :

Voici comment vous pouvez analyser une date à partir d'une chaîne en utilisant C# :

```C#
string dateStr = "15/07/2021";
DateTime parsedDate = DateTime.Parse(dateStr);
Console.WriteLine(parsedDate);
```
L'output sera :
```
2021-07-15 00:00:00
```
Dans cet exemple, `DateTime.Parse` convertit la date en chaîne de caractères en `DateTime`.

## Plongée en profondeur :

L'analyse de la date à partir d'une chaîne de caractères est en place depuis les premiers jours de la programmation comme moyen d'interfacer les dates textuelles avec les systèmes de traitement d'informations numériques.

Il existe d'autres méthodes pour analyser une date à partir d'une chaîne en C#, comme `DateTime.TryParse` et `DateTime.ParseExact`. `DateTime.TryParse` renvoie un booléen indiquant si la conversion a réussi ou non, tandis que `DateTime.ParseExact` permet une correspondance de format plus stricte.

L'implémentation actuelle de l'analyse de date en C# utilise des algorithmes interne pour déchiffrer l'information de la chaîne de caractères, en fonction du format et de la culture en cours. Les détails exacts sont assez complexes et relèvent de la conception interne du .NET framework.

## Voir aussi :

1. [Documentation Microsoft sur DateTime.Parse](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.parse?view=net-5.0)
2. [Documentation Microsoft sur DateTime.TryParse](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.tryparse?view=net-5.0)
3. [Documentation Microsoft sur DateTime.ParseExact](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.parseexact?view=net-5.0)