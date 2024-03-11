---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:18.041201-07:00
description: "\xC5 jobbe med CSV-filer (Comma Separated Values) involverer lesing\
  \ fra eller skriving til rene tekstfiler der datafeltene er adskilt med kommaer.\u2026"
lastmod: '2024-03-11T00:14:14.185437-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med CSV-filer (Comma Separated Values) involverer lesing fra\
  \ eller skriving til rene tekstfiler der datafeltene er adskilt med kommaer.\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å jobbe med CSV-filer (Comma Separated Values) involverer lesing fra eller skriving til rene tekstfiler der datafeltene er adskilt med kommaer. Programmerere utfører ofte denne oppgaven for å lette datautveksling mellom forskjellige programvareapplikasjoner, gitt enkelheten og den brede adopsjonen av CSV-formatet på tvers av ulike programmeringsmiljøer.

## Hvordan:

Visual Basic for Applications (VBA) forenkler arbeidet med CSV-filer gjennom innebygde funksjoner og metoder som sømløst tillater lesing fra og skriving til disse filene. Nedenfor er eksempler som illustrerer grunnleggende operasjoner med CSV-filer.

### Lese en CSV-fil:

```basic
Sub ReadCSV()
    Dim filePath As String
    filePath = "C:\example.csv"
    
    Open filePath For Input As #1
    
    Do Until EOF(1)
        Dim line As String
        Line Input #1, line
        Dim dataFields() As String
        dataFields = Split(line, ",")
        
        'Behandle dataFields-arrayet etter behov
        Debug.Print Join(dataFields, ";") 'Eksempel på output som viser konvertering fra komma til semikolon
    Loop
    
    Close #1
End Sub
```

### Skrive til en CSV-fil:

```basic
Sub WriteCSV()
    Dim filePath As String
    filePath = "C:\output.csv"
    Dim dataToWrite As String
    dataToWrite = "ID,Navn,Alder" & vbCrLf & "1,John Doe,30" & vbCrLf & "2,Jane Doe,29"
    
    Open filePath For Output As #1
    Print #1, dataToWrite
    Close #1
End Sub
```

Eksempel på output i `output.csv`:
```
ID,Navn,Alder
1,John Doe,30
2,Jane Doe,29
```

## Dypdykk

Historisk sett har CSV-filer vært en enkel metode for å lagre tabelldata i tekstformat. Enkelheten i strukturen, hvor hver linje tilsvarer én datarad og hvert felt innenfor en rad er adskilt med et komma, er både CSVs styrke og begrensning. Formatet støtter ikke datatyper på en nativ måte, noe som betyr at all data lagres som tekststrenger, og byrden av å konvertere data til korrekt type faller på programmereren.

I Visual Basic for Applications håndteres arbeid med CSV-filer stort sett gjennom grunnleggende filoperasjoner, som vist i de tidligere eksemplene. Det er ingen direkte støtte for CSV-tolking som i mer moderne språk (for eksempel Pythons csv-modul), noe som gir mer kontroll og bekvemmelighet når man håndterer CSV-data.

For mer komplekse operasjoner eller ved arbeid med store CSV-filer, kan programmerere finne bedre alternativer utenfor ren VBA, som å benytte eksterne biblioteker eller bruke andre programmeringsspråk utstyrt med mer sofistikerte CSV-håndteringsevner. Imidlertid, for enkle oppgaver relatert til CSV-filer, er VBAs rett frem tilnærming ofte tilstrekkelig og enkel å implementere, og tilbyr en rask løsning for Excel-baserte applikasjoner eller annen automatisering av Microsoft Office-programvare.
