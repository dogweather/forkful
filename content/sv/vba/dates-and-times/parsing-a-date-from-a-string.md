---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:43.165925-07:00
description: "Att tolka ett datum fr\xE5n en str\xE4ng i Visual Basic for Applications\
  \ (VBA) handlar om att konvertera text som representerar ett datum till en datumdatatyp.\u2026"
lastmod: '2024-03-11T00:14:11.095782-06:00'
model: gpt-4-0125-preview
summary: "Att tolka ett datum fr\xE5n en str\xE4ng i Visual Basic for Applications\
  \ (VBA) handlar om att konvertera text som representerar ett datum till en datumdatatyp.\u2026"
title: "Analysera ett datum fr\xE5n en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng i Visual Basic for Applications (VBA) handlar om att konvertera text som representerar ett datum till en datumdatatyp. Programmerare gör detta för att kunna hantera datum mer effektivt i sina applikationer, till exempel för jämförelser, beräkningar eller formateringsändamål.

## Hur:

VBA erbjuder ett enkelt sätt att tolka en sträng till ett datum med hjälp av funktionen `CDate` eller `DateValue`. Det är dock avgörande att strängen är i ett erkänt datumformat.

Här är ett grundläggande exempel med `CDate`:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Tolkat Datum: "; parsedDate
End Sub
```

Om du kör denna kod skulle utmatningen i omedelbartfönstret (åtkomligt via `Ctrl+G` i VBA-redigeraren) vara:

```
Tolkat Datum: 4/1/2023 
```

Alternativt kan du använda funktionen `DateValue`, som är mer specifik för datum (ignorerar tidsdelen):

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "April 1, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Tolkat Datum med DateValue: "; parsedDate
End Sub
```

Ett exempel på utmatning för detta skulle liknande visas i omedelbartfönstret:

```
Tolkat Datum med DateValue: 4/1/2023
```

Kom ihåg att framgången med tolkningen beror på att datumformatet på strängen matchar systemets eller applikationens inställningar.

## Fördjupning

Internt, när VBA tolkar en sträng till ett datum, använder det de regionala inställningarna för Windows operativsystem för att tolka datumformatet. Detta är avgörande att förstå eftersom en datumsträng som tolkas perfekt på ett system kan orsaka ett fel på ett annat om de använder olika datum/tidsinställningar.

Historiskt sett har hanteringen av datum varit en vanlig källa till buggar i applikationer, särskilt de som används internationellt. Detta beroende av regionala inställningar i VBA är varför vissa kan överväga alternativ som ISO 8601-formatet (t.ex. "ÅÅÅÅ-MM-DD") för entydig datumrepresentation och tolkning över olika system. Tyvärr stöder inte VBA nativt ISO 8601, och manuell tolkning skulle vara nödvändig för strikt efterlevnad.

För komplex datumtolkning utöver vad `CDate` eller `DateValue` kan hantera, eller för att säkerställa konsekvent tolkning oavsett systemets lokala inställningar, kan programmerare använda sig av anpassade tolkningsfunktioner. Dessa kan innefatta att dela upp datumsträngen i komponenter (år, månad, dag) och konstruera ett datum med hjälp av `DateSerial`-funktionen. Andra kan välja kraftfullare språk eller bibliotek som är utformade med internationalisering i åtanke för sådana uppgifter.
