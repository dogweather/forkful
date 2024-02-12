---
title:                "Een datum ontleden uit een string"
aliases: - /nl/vba/parsing-a-date-from-a-string.md
date:                  2024-02-01T21:57:30.374321-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum ontleden uit een string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/vba/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum parsen uit een string in Visual Basic for Applications (VBA) gaat over het omzetten van tekst die een datum voorstelt naar een datum gegevenstype. Programmeurs doen dit om data effectiever te manipuleren in hun toepassingen, zoals voor vergelijkingen, berekeningen of formatteerdoeleinden.

## Hoe:

VBA biedt een eenvoudige manier om een string naar een datum te parsen met behulp van de `CDate`-functie of de `DateValue`-functie. Het is echter cruciaal dat de string een herkenbaar datumformaat heeft.

Hier is een basisvoorbeeld met `CDate`:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Geparseerde Datum: "; parsedDate
End Sub
```

Als u deze code uitvoert, zou de output in het Direct Venster (toegankelijk via `Ctrl+G` in de VBA-editor) zijn:

```
Geparseerde Datum: 4/1/2023 
```

Als alternatief kunt u de `DateValue`-functie gebruiken, die specifieker is voor data (en het tijddeel negeert):

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "April 1, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Geparseerde Datum met DateValue: "; parsedDate
End Sub
```

Een voorbeelduitvoer hiervoor zou eveneens getoond worden in het Direct Venster:

```
Geparseerde Datum met DateValue: 4/1/2023
```

Houd er rekening mee dat het succes van het parsen afhangt van het feit of het datumformaat van de string overeenkomt met systeem- of applicatie-instellingen.

## Diep Duiken

Intern, wanneer VBA een string naar een datum parseert, gebruikt het de regionale instellingen van het Windows-besturingssysteem om het datumformaat te interpreteren. Dit is cruciaal om te begrijpen, omdat een datumstring die perfect parseert op het ene systeem, een fout kan veroorzaken op een ander als ze verschillende datum-/tijdinstellingen gebruiken.

Historisch gezien is het omgaan met data een veelvoorkomende bron van bugs in toepassingen geweest, vooral die internationaal gebruikt worden. Deze afhankelijkheid van regionale instellingen in VBA is waarom sommigen alternatieven zoals het ISO 8601-formaat (bijv. "YYYY-MM-DD") zouden overwegen voor een ondubbelzinnige vertegenwoordiging en parsing van data over verschillende systemen heen. Helaas ondersteunt VBA ISO 8601 niet van nature, en handmatig parsen zou nodig zijn voor strikte naleving.

Voor complexe datum-parsing buiten wat `CDate` of `DateValue` kan afhandelen, of om consistente parsing ongeacht systeemlocale-instellingen te verzekeren, kunnen programmeurs hun toevlucht nemen tot aangepaste parse-functies. Deze kunnen het splitsen van de datumstring in componenten (jaar, maand, dag) en het construeren van een datum met behulp van de `DateSerial`-functie omvatten. Anderen kunnen kiezen voor krachtigere talen of bibliotheken die met internationalisering in gedachten zijn ontworpen voor dergelijke taken.
