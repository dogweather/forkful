---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:51.565220-07:00
description: "Code in functies opdelen is als LEGO-stenen sorteren in bakken\u2014\
  het maakt het vinden en gebruiken ervan gemakkelijker. We doen dit om herhaling\
  \ te\u2026"
lastmod: '2024-02-25T18:49:48.155145-07:00'
model: gpt-4-0125-preview
summary: "Code in functies opdelen is als LEGO-stenen sorteren in bakken\u2014het\
  \ maakt het vinden en gebruiken ervan gemakkelijker. We doen dit om herhaling te\u2026"
title: Code organiseren in functies
---

{{< edit_this_page >}}

## Wat & Waarom?
Code in functies opdelen is als LEGO-stenen sorteren in bakken—het maakt het vinden en gebruiken ervan gemakkelijker. We doen dit om herhaling te voorkomen, begrip te vereenvoudigen en onderhoud minder hoofdpijn te veroorzaken.

## Hoe te:
Stel je voor dat je code hebt die meerdere keren een begroeting afdrukt. Zonder functies is het een rommeltje. Met functies is het netjes.

```C#
// Zonder functies - herhalend
Console.WriteLine("Hallo, Amy!");
Console.WriteLine("Hallo, Bob!");
Console.WriteLine("Hallo, Charlie!");

// Met functies - schoner
void Groet(string naam) {
    Console.WriteLine($"Hallo, {naam}!");
}

Groet("Amy");
Groet("Bob");
Groet("Charlie");
```

De uitvoer is hetzelfde, maar de tweede versie is veel netter.

## Diepgaand
Lang geleden, in de dagen van assemblytaal, sprong je met GOTO naar verschillende codestukken—chaotisch en moeilijk te volgen. Functies zijn een grote sprong voorwaarts, zoals georganiseerde lades in een gereedschapskist. Alternatieven? Zeker. Je hebt methoden, die functies zijn in een klassecontext. Dan zijn er nog lambda's en inline functies voor snelle, eenmalige taken.

Over implementatie—kleine, gerichte functies zijn goud waard. Ze zijn makkelijker te testen en debuggen. Grote functies met veel verantwoordelijkheden kunnen monsters worden, en verdienen dan de dubieuze titel "spaghetticode". Hou je aan één taak per functie; je zult jezelf later dankbaar zijn.

## Zie Ook
Voor meer over functies en beste praktijken, bekijk:

- Clean Code van Robert C. Martin: Principes om je functies netjes te houden.
- Refactoring van Martin Fowler: Manieren om bestaande code te verbeteren.
- Microsoft C# Gids over Methoden: https://docs.microsoft.com/nl-nl/dotnet/csharp/programming-guide/classes-and-structs/methods
