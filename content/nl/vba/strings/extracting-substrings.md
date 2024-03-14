---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:04.864567-07:00
description: "Het extraheren van substrings in Visual Basic voor Applications (VBA)\
  \ omvat het isoleren van specifieke delen van een string op basis van gegeven\u2026"
lastmod: '2024-03-13T22:44:50.625306-06:00'
model: gpt-4-0125-preview
summary: "Het extraheren van substrings in Visual Basic voor Applications (VBA) omvat\
  \ het isoleren van specifieke delen van een string op basis van gegeven\u2026"
title: Substrings extraheren
---

{{< edit_this_page >}}

## Wat & Waarom?

Het extraheren van substrings in Visual Basic voor Applications (VBA) omvat het isoleren van specifieke delen van een string op basis van gegeven criteria. Programmeurs doen dit voor taken zoals data-analyse, validatie en formattering, waarbij het manipuleren en extraheren van informatie uit tekstgegevens cruciaal is.

## Hoe:

In VBA gebruik je voornamelijk de functies `Mid`, `Left` en `Right` om substrings te extraheren. Hieronder verkennen we deze functies met voorbeelden:

1. **Mid**: Extraheert een substring van een string vanaf een opgegeven positie.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim resultaat As String
   resultaat = Mid(exampleString, 7, 5)
   Debug.Print resultaat  ' Uitvoer: World
   ```

2. **Left**: Extraheert een substring van de linkerkant van de string, tot een opgegeven aantal tekens.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim resultaat As String
   resultaat = Left(exampleString, 5)
   Debug.Print resultaat  ' Uitvoer: Hello
   ```

3. **Right**: Extraheert een substring van de rechterkant van de string, tot een opgegeven aantal tekens.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim resultaat As String
   resultaat = Right(exampleString, 5)
   Debug.Print resultaat  ' Uitvoer: World
   ```

Deze fundamentele functies vormen de basis van substringextractie in VBA en bieden robuuste en eenvoudige benaderingen voor stringmanipulatie.

## Diepe Duik:

Historisch gezien is het vermogen om strings in programmering te manipuleren essentieel geweest, waarbij BASIC (de voorloper van VBA) een van de eersten was die deze mogelijkheid democratiseerde in de vroege dagen van personal computing. De functies `Mid`, `Left` en `Right` in VBA erven deze erfenis en bieden een vereenvoudigde interface voor moderne programmeurs.

Hoewel deze functies voor veel taken zeer effectief zijn, heeft de opkomst van Reguliere Expressies in nieuwere talen een krachtigere en flexibelere manier geboden om met tekst te werken. Ondanks dit, maken de directe eenvoud en beschikbaarheid van de traditionele VBA-substringfuncties ze perfect geschikt voor snelle taken en diegenen die nieuw zijn in programmeren.

Voor complexere parsing- en zoekoperaties binnen strings ondersteunt VBA ook patroonmatching via de `Like`-operator en Reguliere Expressies via het `VBScript.RegExp`-object, hoewel deze wat meer opzet en begrip vereisen om effectief te gebruiken. Hoewel deze hulpmiddelen grotere kracht bieden, zorgen de directe aard van `Mid`, `Left` en `Right` ervoor dat ze hun relevantie en nut in veel VBA-programma’s blijven behouden.
