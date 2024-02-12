---
title:                "Een nieuw project starten"
aliases:
- /nl/c-sharp/starting-a-new-project.md
date:                  2024-01-28T22:08:13.690683-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een nieuw C# project starten betekent het opzetten van een nieuwe oplossing en projectbestanden die je code structureren. Programmeurs beginnen nieuwe projecten om ideeën om te zetten in software, problemen op te lossen of technologie te verkennen.

## Hoe:
Laten we onze mouwen opstropen en wat code onder handen nemen. Ga ervan uit dat je .NET 6 of later hebt - dat is de laatste versie op het moment van schrijven. Je gebruikt de .NET CLI hiervoor.

Maak een nieuwe console-app:
```C#
dotnet new console -o MijnNieuwProject
```
Ga naar je projectmap:
```C#
cd MijnNieuwProject
```
Voer je verse, standaard Hello World uit:
```C#
dotnet run
```
Je zou moeten zien:
```
Hello, World!
```
Je nieuwe project is van de grond!

## Diepere Duik
Vroeger zou je waarschijnlijk Visual Studio opstarten en door een wizard klikken. Niet meer - nu is de .NET CLI de weg te gaan. Het is snel en gaat niet veel uit van je ontwikkelomgeving.

Alternatieven? Zeker. Visual Studio is er nog steeds voor een GUI-ervaring. Rider en Visual Studio Code zijn ook solide keuzes. Maar de CLI? Het draait allemaal om die strakke, efficiënte scripting vibe.

Implementatie details? Je `.csproj` bestand houdt de sleutels tot het koninkrijk. Het is XML, maar maak je geen zorgen - het zorgt grotendeels voor zichzelf. Hier ligt informatie die je bouwproces nodig heeft - doelframework, afhankelijkheden, projectreferenties, al het goede spul.

## Zie Ook
- [Officiële .NET CLI Documentatie](https://docs.microsoft.com/en-us/dotnet/core/tools/)
- [Visual Studio Productpagina](https://visualstudio.microsoft.com/)
- [Overzicht van .NET Project SDK](https://docs.microsoft.com/en-us/dotnet/core/project-sdk/overview)
