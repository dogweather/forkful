---
aliases:
- /sv/vba/rounding-numbers/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:44.202805-07:00
description: "Att avrunda tal i programmering handlar om att approximera ett tal till\
  \ dess n\xE4rmaste heltal eller till ett visst antal decimalplatser. Programmerare\u2026"
lastmod: 2024-02-18 23:08:51.621081
model: gpt-4-0125-preview
summary: "Att avrunda tal i programmering handlar om att approximera ett tal till\
  \ dess n\xE4rmaste heltal eller till ett visst antal decimalplatser. Programmerare\u2026"
title: Avrundning av nummer
---

{{< edit_this_page >}}

## Vad & Varför?

Att avrunda tal i programmering handlar om att approximera ett tal till dess närmaste heltal eller till ett visst antal decimalplatser. Programmerare avrundar tal för att förenkla siffror, förbättra läsbarheten eller uppfylla specifika numeriska kriterier i beräkningar, särskilt i finansiella beräkningar där precision är viktigt.

## Hur man gör:

I Visual Basic for Applications (VBA) kan avrundning uppnås med hjälp av flera funktioner, var och en lämpad för specifika scenarier. Här är de vanligast använda funktionerna med exempel:

1. **Round-funktionen**:
   `Round`-funktionen avrundar ett tal till ett specificerat antal siffror.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' Utdata: 3.14
   MsgBox roundedNumber
   ```
   
2. **Int- och Fix-funktionerna**:
   Både `Int`- och `Fix`-funktionerna används för att avrunda tal nedåt till närmaste heltal, men de beter sig olika med negativa tal.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' Utdata: -4
   fixRounded = Fix(-3.14159)  ' Utdata: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Ceiling- och Floor-funktionerna**:
   VBA saknar inbyggda `Ceiling`- och `Floor`-funktioner som finns i andra språk. För att simulera detta, använd `Application.WorksheetFunction.Ceiling_Math` och `Application.WorksheetFunction.Floor_Math` för Excel VBA.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Utdata: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' Utdata: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## Fördjupning

`Round`-funktionen i VBA är i grunden annorlunda från avrundningsmetoder i andra språk på grund av dess användning av **Bankers avrundning**. Bankers avrundning avrundar till det närmaste jämna talet när det är exakt mitt emellan två tal, vilket minskar bias i beräkningar över en stor datamängd och ger ett mer statistiskt signifikant resultat. Detta kan dock leda till oväntat beteende för dem som inte är bekanta med det, särskilt när exakt precision förväntas i varje fall.

I motsats till detta använder många programmeringsspråk och system "aritmetisk avrundning" eller "halv-upp-avrundning", där ett tal som ligger exakt mitt emellan två möjliga avrundade värden alltid avrundas uppåt. När man översätter eller porterar kod från andra språk till VBA måste programmerare ha dessa skillnader i åtanke för att undvika subtila buggar eller felaktigheter i finansiella och statistiska tillämpningar.

Även om VBA erbjuder en mängd funktioner för avrundning, belyser avsaknaden av `Ceiling`- och `Floor`-funktioner (utan att behöva tillgripa Excels WorksheetFunction) en begränsning i dess inbyggda kapaciteter. Programmerare som kommer från mer funktionsrika språk kan tycka att dessa utelämnanden är obekväma och kan behöva implementera egna lösningar eller anpassa sina beräkningar för att använda tillgängliga funktioner. Trots dessa begränsningar kan förståelse och korrekt användning av VBA:s avrundningsfunktioner hjälpa till att säkerställa att numeriska beräkningar är både korrekta och uppfyller kraven i de flesta tillämpningar.
