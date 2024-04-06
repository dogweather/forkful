---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:22.415154-07:00
description: "Hvordan: I VBA kan du bruke `Replace`-funksjonen eller regul\xE6re uttrykk\
  \ for \xE5 slette tegn som matcher et m\xF8nster. Her er eksempler p\xE5 begge metodene."
lastmod: '2024-04-05T21:53:41.579915-06:00'
model: gpt-4-0125-preview
summary: "I VBA kan du bruke `Replace`-funksjonen eller regul\xE6re uttrykk for \xE5\
  \ slette tegn som matcher et m\xF8nster."
title: "Slette tegn som samsvarer med et m\xF8nster"
weight: 5
---

## Hvordan:
I VBA kan du bruke `Replace`-funksjonen eller regulære uttrykk for å slette tegn som matcher et mønster. Her er eksempler på begge metodene:

### Bruk av `Replace`-Funksjonen
`Replace`-funksjonen er grei for å fjerne spesifikke tegn eller sekvenser.

```basic
Sub DeleteSpecificChars()
    Dim originalString As String
    originalString = "123-ABC-456-XYZ"
    
    ' Fjerner bindestreker
    Dim resultString As String
    resultString = Replace(originalString, "-", "")
    
    Debug.Print originalString ' Før: 123-ABC-456-XYZ
    Debug.Print resultString ' Etter: 123ABC456XYZ
End Sub
```

### Bruk av Regulære Uttrykk
For mer komplekse mønstre, tilbyr regulære uttrykk et kraftig alternativ.

Først, aktiver Microsoft VBScript Regular Expressions-biblioteket via Verktøy > Referanser i Visual Basic-editoren.


```basic
Sub DeletePatternChars()
    Dim regEx As Object
    Set regEx = CreateObject("VBScript.RegExp")
    
    Dim strPattern As String
    strPattern = "\d" ' Mønster for å matche alle sifre
    
    With regEx
        .Global = True
        .IgnoreCase = True
        .Pattern = strPattern
    End With
    
    Dim originalString As String
    originalString = "Remove 123 and 456"
    
    ' Bruker Replace-metoden for å slette treff
    Dim resultString As String
    resultString = regEx.Replace(originalString, "")
    
    Debug.Print originalString ' Før: Remove 123 and 456
    Debug.Print resultString ' Etter: Remove  and 
End Sub
```

## Dypdykk
Historisk sett har mønstermatching og manipulering av strenger i VBA vært noe begrenset, spesielt sammenliknet med mer moderne programmeringsspråk som tilbyr omfattende standardbiblioteker for disse oppgavene. `Replace`-funksjonen er enkel og effektiv for direkte substitusjoner, men mangler fleksibiliteten for mer kompleks mønstermatching. Her kommer regulære uttrykk (RegEx) inn, som tilbyr en mye rikere syntaks for mønstermatching og manipulering av strenger. Likevel krever arbeid med RegEx i VBA ekstra oppsett, som å aktivere referansen til Microsoft VBScript Regular Expressions, noe som kan være en barriere for nye brukere.

Til tross for disse begrensningene, var introduksjonen av RegEx-støtte i VBA et betydelig skritt framover, og tilbød et kraftigere verktøy for programmerere som arbeider med tekstbehandling. I mer komplekse scenarioer hvor innebygde strengfunksjoner kommer til kort, tilbyr regulære uttrykk et allsidig og kraftig alternativ.

Det er verdt å merke seg at for de som arbeider i miljøer eller prosjekter hvor ytelse er kritisk, kan det å bruke eksterne biblioteker eller integrere med andre programmeringsspråk gi bedre ytelse og flere funksjoner. Likevel, for mange dag-til-dag-oppgaver i VBA, forblir disse innebygde metodene et praktisk og tilgjengelig valg.
