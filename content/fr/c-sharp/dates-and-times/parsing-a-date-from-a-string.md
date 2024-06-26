---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:45.603181-07:00
description: "Comment faire : **Analyse Basique :** Les m\xE9thodes `DateTime.Parse`\
  \ et `DateTime.TryParse` sont les options privil\xE9gi\xE9es pour convertir une\
  \ cha\xEEne en\u2026"
lastmod: '2024-04-05T21:53:59.282982-06:00'
model: gpt-4-0125-preview
summary: "**Analyse Basique :** Les m\xE9thodes `DateTime.Parse` et `DateTime.TryParse`\
  \ sont les options privil\xE9gi\xE9es pour convertir une cha\xEEne en `DateTime`."
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
weight: 30
---

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
