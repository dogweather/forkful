---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:45.603181-07:00
description: "Analyser une date \xE0 partir d'une cha\xEEne en C# consiste \xE0 convertir\
  \ des repr\xE9sentations textuelles de dates et d'heures en un objet `DateTime`.\
  \ Cela est\u2026"
lastmod: '2024-02-25T18:49:54.524107-07:00'
model: gpt-4-0125-preview
summary: "Analyser une date \xE0 partir d'une cha\xEEne en C# consiste \xE0 convertir\
  \ des repr\xE9sentations textuelles de dates et d'heures en un objet `DateTime`.\
  \ Cela est\u2026"
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Analyser une date à partir d'une chaîne en C# consiste à convertir des représentations textuelles de dates et d'heures en un objet `DateTime`. Cela est essentiel pour les applications qui doivent manipuler, stocker ou afficher des dates et des heures dans différents formats, comme les applications de planification, les processeurs de journaux ou tout système gérant des entrées de dates provenant d'utilisateurs ou de sources externes.

## Comment faire :

**Analyse Basique :**

Les méthodes `DateTime.Parse` et `DateTime.TryParse` sont les options privilégiées pour convertir une chaîne en `DateTime`. Voici un exemple rapide :

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Analyse réussie : {parsedDate}");
}
else
{
    Console.WriteLine("Échec de l'analyse.");
}
// Sortie : Analyse réussie : 12/04/2023 00:00:00
```

**Spécification d'une Culture :**

Parfois, vous devez analyser une chaîne de date qui est dans un format de culture spécifique. Vous pouvez y parvenir en utilisant la classe `CultureInfo` :

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Sortie : 12/04/2023 00:00:00
```

**Analyse Exacte avec un Format Spécifique :**

Pour les scénarios où les dates sont dans un format spécifique qui peut ne pas être standard, `DateTime.ParseExact` est très utile :

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Sortie : 12/04/2023 00:00:00
```

**Utilisation de NodaTime :**

Pour une analyse de date et d'heure encore plus robuste, envisagez d'utiliser la bibliothèque tierce populaire NodaTime. Elle offre une gamme plus large de capacités de manipulation de date/heure :

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Échec de l'analyse.");
}
```

NodaTime offre un soutien étendu pour les fuseaux horaires, les concepts de période et de durée, et de nombreux systèmes de calendrier différents, ce qui en fait un choix puissant pour la manipulation complexe de dates et d'heures dans les applications .NET.
