---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:08.731689-07:00
description: "Het genereren van willekeurige getallen in Visual Basic for Applications\
  \ (VBA) stelt programma's in staat processen met elementen van kans of\u2026"
lastmod: '2024-03-13T22:44:50.632411-06:00'
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in Visual Basic for Applications\
  \ (VBA) stelt programma's in staat processen met elementen van kans of\u2026"
title: Willekeurige getallen genereren
weight: 12
---

## Wat & Waarom?

Het genereren van willekeurige getallen in Visual Basic for Applications (VBA) stelt programma's in staat processen met elementen van kans of variabiliteit te simuleren, zoals dobbelsteenworpen of het steekproefsgewijs verzamelen van gegevens. Programmeurs gebruiken deze technieken om modellen, spellen of simulaties te ontwikkelen waarbij voorspelbare uitkomsten onrealistisch of minder nuttig zouden zijn.

## Hoe te:

In VBA wordt de `Rnd`-functie gebruikt om willekeurige getallen te genereren. Standaard genereert `Rnd` een enkelvoudig-nauwkeurig zwevendekommagetal dat groter dan of gelijk aan 0 en kleiner dan 1 is. Hier zijn een paar stappen en voorbeelden om effectief willekeurige getallen te gebruiken:

1. **Eenvoudig Willekeurig Getal:**
   Om een basis willekeurig getal te genereren, hoef je alleen maar `Rnd()` aan te roepen:

   ```vb
   Sub GenerateRandomNumber()
       Dim randomNumber As Single
       randomNumber = Rnd() ' Willekeurig getal tussen 0 en 1
       MsgBox randomNumber
   End Sub
   ```

2. **De Seed Instellen:**
   De `Randomize` instructie initialiseert de generator voor willekeurige getallen, wat cruciaal kan zijn om verschillende uitkomsten te garanderen elke keer dat je VBA-code draait:

   ```vb
   Sub SeedRandomNumber()
       Randomize
       Dim randomNumber As Single
       randomNumber = Rnd()
       MsgBox randomNumber
   End Sub
   ```

3. **Getallen Genereren binnen een Bereik:**
   Vaak wil je een willekeurig getal binnen een specifiek bereik. Hier is hoe je een getal tussen 1 en 100 genereert:

   ```vb
   Sub RandomNumberInRange()
       Randomize
       Dim randomNumber As Integer
       randomNumber = Int((100 * Rnd()) + 1) ' Willekeurig getal tussen 1 en 100
       MsgBox randomNumber
   End Sub
   ```

### Voorbeelduitvoer:
Na het draaien van `RandomNumberInRange`, zie je misschien een berichtvenster dat een getal zoals `45` weergeeft.

## Diepgaande Duik:

De `Rnd`-functie in VBA, hoewel gemakkelijk te gebruiken, genereert eigenlijk pseudo-willekeurige getallen gebaseerd op een deterministisch algoritme. Dit betekent dat de sequenties van getallen die het produceert niet echt willekeurig zijn, maar vaak volstaan voor veelvoorkomende taken die stochastische processen nodig hebben.

Historisch gezien gaat het vermogen om willekeurige getallen te genereren in VBA terug naar vroege versies van Basic, zich over tijd aanpassend om functies zoals `Randomize` te omvatten om de willekeurigheid te verbeteren door het algoritme te voorzien van een startpunt. Echter, voor toepassingen die een hoog niveau van willekeurigheid vereisen zoals veilige cryptografische operaties, is VBA’s `Rnd` misschien niet het beste hulpmiddel. Alternatieven in robuustere programmeeromgevingen of talen die zijn ontworpen met cryptografie in gedachten, zoals Python's `secrets` module of Java's `SecureRandom`, moeten worden overwogen.

Ondanks de beperkingen blijft de eenvoud en toegankelijkheid van het genereren van willekeurige getallen in VBA het een waardevol hulpmiddel maken voor een breed scala aan lichtere toepassingen, simulatiewerk en educatieve doeleinden.
