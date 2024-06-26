---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:10.278545-07:00
description: "Hvordan: I Visual Basic for Applications (VBA), kan avrunding oppn\xE5\
  s ved bruk av flere funksjoner, hver egnet for spesifikke scenarioer. Her er de\
  \ mest\u2026"
lastmod: '2024-03-13T22:44:40.612902-06:00'
model: gpt-4-0125-preview
summary: "I Visual Basic for Applications (VBA), kan avrunding oppn\xE5s ved bruk\
  \ av flere funksjoner, hver egnet for spesifikke scenarioer."
title: Avrunding av tall
weight: 13
---

## Hvordan:
I Visual Basic for Applications (VBA), kan avrunding oppnås ved bruk av flere funksjoner, hver egnet for spesifikke scenarioer. Her er de mest brukte funksjonene med eksempler:

1. **Round-funksjonen**:
   `Round`-funksjonen runder av et tall til et spesifisert antall sifre.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' Utdata: 3.14
   MsgBox roundedNumber
   ```
   
2. **Int- og Fix-funksjonene**:
   Både `Int`- og `Fix`-funksjonene brukes til å runde ned tall til nærmeste heltall, men de oppfører seg forskjellig med negative tall.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' Utdata: -4
   fixRounded = Fix(-3.14159)  ' Utdata: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Ceiling- og Floor-funksjonene**:
   VBA mangler innebygde `Ceiling`- og `Floor`-funksjoner som finnes i andre språk. For å simulere dette, bruk `Application.WorksheetFunction.Ceiling_Math` og `Application.WorksheetFunction.Floor_Math` for Excel VBA.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Utdata: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' Utdata: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## Dybdestudie
`Round`-funksjonen i VBA er vesentlig forskjellig fra avrundingsmetoder i andre språk på grunn av dens bruk av **Bankers avrunding**. Bankers avrunding runder til det nærmeste jevne tallet når man er nøyaktig midt mellom to tall, noe som reduserer skjevhet i beregninger over et stort datasett og gir et mer statistisk signifikant resultat. Dette kan imidlertid føre til uventet oppførsel for de som er ukjente med det, spesielt når presis nøyaktighet er forventet i alle tilfeller.

I motsetning bruker mange programmeringsspråk og systemer "aritmetisk avrunding" eller "halvopp-avrunding," der et tall nøyaktig midt mellom to mulige avrundede verdier alltid avrundes opp. Når man oversetter eller overfører kode fra andre språk til VBA, må programmerere ha disse forskjellene i tankene for å unngå subtile feil eller unøyaktigheter i finansielle og statistiske applikasjoner.

Selv om VBA tilbyr en rekke funksjoner for avrunding, fremhever fraværet av `Ceiling`- og `Floor`-funksjoner (uten å ty til Excels WorksheetFunction) en begrensning i dens medfødte kapasiteter. Programmerere som kommer fra mer funksjonsrike språk kan finne disse utelatelsene upraktiske og kan trenge å implementere egendefinerte løsninger eller tilpasse beregningene sine for å bruke tilgjengelige funksjoner. Til tross for disse begrensningene, kan forståelse og bruk av VBAs avrundingsfunksjoner korrekt bidra til å sikre at numeriske beregninger er både nøyaktige og oppfyller kravene til de fleste applikasjoner.
