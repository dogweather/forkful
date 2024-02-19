---
aliases:
- /nl/vba/getting-the-current-date/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:35.628910-07:00
description: "In Visual Basic for Applications (VBA), is het ophalen van de huidige\
  \ datum een veelvoorkomende taak die programmeurs in staat stelt om dynamisch met\u2026"
lastmod: 2024-02-18 23:09:01.677368
model: gpt-4-0125-preview
summary: "In Visual Basic for Applications (VBA), is het ophalen van de huidige datum\
  \ een veelvoorkomende taak die programmeurs in staat stelt om dynamisch met\u2026"
title: Het verkrijgen van de huidige datum
---

{{< edit_this_page >}}

## Wat & Waarom?

In Visual Basic for Applications (VBA), is het ophalen van de huidige datum een veelvoorkomende taak die programmeurs in staat stelt om dynamisch met datums in hun macro's of applicaties te werken. Deze functionaliteit is cruciaal voor bewerkingen zoals loggen, het timestampen van transacties of het uitvoeren van op data gebaseerde berekeningen.

## Hoe te:

Het ophalen van de huidige datum in VBA is eenvoudig, met gebruik van de `Date` functie, terwijl de `Now` functie zowel de huidige datum als tijd biedt. Hier is hoe je met beide kunt werken:

```vb
Sub GetCurrentDate()
    ' De Date functie gebruiken om de huidige datum te krijgen
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "Huidige Datum: "; currentDate
    
    ' De Now functie gebruiken om de huidige datum en tijd te krijgen
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "Huidige Datum en Tijd: "; currentDateTime
End Sub
```

Wanneer je deze macro draait, geeft de `Debug.Print` methode de huidige datum en de huidige datum en tijd uit in het Onmiddellijk Venster in de VBA-editor. Bijvoorbeeld:

```
Huidige Datum: 4/12/2023
Huidige Datum en Tijd: 4/12/2023 15:45:22
```

Houd er rekening mee dat het datumformaat kan variëren op basis van de systeeminstellingen van de computer van de gebruiker.

## Diepgaande Duik

De `Date` en `Now` functies omvatten de complexiteit van het omgaan met datum en tijd in Visual Basic for Applications, en bieden een applicatieniveau abstractie die werken met datums eenvoudig en intuïtief maakt. Het omgaan met datum en tijd in programmering is historisch gezien beladen geweest met uitdagingen, inclusief het hanteren van verschillende tijdzones, wijzigingen in zomertijd en diverse datumformaten.

In VBA, vertrouwen deze functies op de onderliggende systeemdatum en -tijd, wat betekent dat ze worden beïnvloed door de locale en systeeminstellingen van de gebruiker. Het is een dubbelzijdig zwaard dat zorgt voor consistentie met de omgeving van de gebruiker, maar ook zorgvuldige afhandeling van lokalisatie en aanpassingen van tijdzones in wereldwijde applicaties noodzakelijk maakt.

Hoewel de datum- en tijdfuncties van VBA perfect geschikt zijn voor veel toepassingen, vooral binnen het bereik van Office-automatisering, kunnen ze de precisie of granulariteit missen die vereist is voor complexere toepassingen zoals high-frequency trading systemen of wetenschappelijke simulaties. In dergelijke gevallen kunnen andere programmeeromgevingen of talen zoals Python of C# geavanceerdere bibliotheken voor datum- en tijdmanipulatie bieden.

Desalniettemin, voor de overgrote meerderheid van taken met betrekking tot datums en tijden in de context van Excel, Word, of andere Office-applicaties, bieden VBA's `Date` en `Now` functies een balans van eenvoud, prestaties, en gebruiksgemak die moeilijk te verslaan is.
