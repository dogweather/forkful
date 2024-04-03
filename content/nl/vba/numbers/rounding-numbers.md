---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:14.240737-07:00
description: "Hoe te: In Visual Basic for Applications (VBA) kan afronden worden bereikt\
  \ met behulp van verschillende functies, elk geschikt voor specifieke scenario's.\u2026"
lastmod: '2024-03-13T22:44:50.631351-06:00'
model: gpt-4-0125-preview
summary: In Visual Basic for Applications (VBA) kan afronden worden bereikt met behulp
  van verschillende functies, elk geschikt voor specifieke scenario's.
title: Afronden van getallen
weight: 13
---

## Hoe te:
In Visual Basic for Applications (VBA) kan afronden worden bereikt met behulp van verschillende functies, elk geschikt voor specifieke scenario's. Hier zijn de meest gebruikte functies met voorbeelden:

1. **Round-functie**:
   De `Round`-functie rondt een getal af op een opgegeven aantal cijfers.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' Uitvoer: 3.14
   MsgBox roundedNumber
   ```
   
2. **Int- en Fix-functies**:
   Zowel de `Int`- als de `Fix`-functie worden gebruikt om getallen naar beneden af te ronden naar het dichtstbijzijnde geheel getal, maar ze gedragen zich anders bij negatieve getallen.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' Uitvoer: -4
   fixRounded = Fix(-3.14159)  ' Uitvoer: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Ceiling- en Floor-functies**:
   VBA mist ingebouwde `Ceiling`- en `Floor`-functies die in andere talen worden gevonden. Om dit te simuleren, gebruik je `Application.WorksheetFunction.Ceiling_Math` en `Application.WorksheetFunction.Floor_Math` voor Excel VBA.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Uitvoer: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' Uitvoer: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## Diepgaand
De `Round`-functie in VBA verschilt inherent van afrondingsmethoden in andere talen door het gebruik van **Bankiersafronding**. Bankiersafronding rondt af naar het dichtstbijzijnde even getal wanneer precies halverwege tussen twee getallen, waardoor de bias in berekeningen over een grote dataset wordt verminderd en een statistisch significant resultaat wordt verkregen. Dit kan echter leiden tot onverwacht gedrag voor degenen die er niet bekend mee zijn, vooral wanneer integrale precisie in elk geval wordt verwacht.

In tegenstelling tot veel programmeertalen en systemen gebruiken "arithmetisch afronden" of "half-omhoog afronden", waarbij een getal dat precies halverwege tussen twee mogelijke afgeronde waarden is, altijd wordt afgerond naar boven. Wanneer je code vertaalt of overzet van andere talen naar VBA, moeten programmeurs deze verschillen in gedachten houden om subtiele bugs of onnauwkeurigheden in financiële en statistische toepassingen te vermijden.

Hoewel VBA een verscheidenheid aan functies voor afronden biedt, benadrukt de afwezigheid van `Ceiling`- en `Floor`-functies (zonder terug te vallen op Excel's WorksheetFunction) een beperking in zijn native capaciteiten. Programmeurs die afkomstig zijn uit talen met meer functies vinden deze weglatingen misschien lastig en moeten mogelijk aangepaste oplossingen implementeren of hun berekeningen aanpassen om beschikbare functies te gebruiken. Ondanks deze beperkingen kan het correct begrijpen en gebruiken van VBA's afrondingsfuncties helpen om ervoor te zorgen dat numerieke berekeningen zowel nauwkeurig zijn als voldoen aan de vereisten van de meeste toepassingen.
