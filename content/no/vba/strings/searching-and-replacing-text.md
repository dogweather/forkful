---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:26.126638-07:00
description: "Hvordan: I VBA kan s\xF8king og erstatning av tekst utf\xF8res ved hjelp\
  \ av `Replace`-funksjonen eller gjennom spesifikke objektmodeller i applikasjoner\
  \ som\u2026"
lastmod: '2024-03-13T22:44:40.601764-06:00'
model: gpt-4-0125-preview
summary: "I VBA kan s\xF8king og erstatning av tekst utf\xF8res ved hjelp av `Replace`-funksjonen\
  \ eller gjennom spesifikke objektmodeller i applikasjoner som Excel eller Word."
title: "S\xF8ke og erstatte tekst"
weight: 10
---

## Hvordan:
I VBA kan søking og erstatning av tekst utføres ved hjelp av `Replace`-funksjonen eller gjennom spesifikke objektmodeller i applikasjoner som Excel eller Word. Nedenfor er eksempler som illustrerer begge tilnærminger.

### Bruke `Replace`-funksjonen:
`Replace`-funksjonen er ukomplisert for enkle teksterstatninger. Den har formen `Replace(uttrykk, finn, erstattMed[, start[, antall[, sammenlign]]])`.

Eksempel:
```vb
Dim originalTekst As String
Dim nyTekst As String

originalTekst = "Hei, Verden! Programmering i VBA er gøy."
nyTekst = Replace(originalTekst, "Verden", "Alle")

Debug.Print nyTekst
```
Output:
```
Hei, Alle! Programmering i VBA er gøy.
```

### Søke og erstatte i Excel:
For Excel, kan du bruke `Range.Replace`-metoden som tilbyr mer kontroll, som f.eks. følsomhet for store og små bokstaver og hele ord-erstatninger.

Eksempel:
```vb
Sub ErstattTekstIExcel()
    Dim ws As Worksheet
    Set ws = ThisWorkbook.Sheets("Sheet1")

    With ws.Range("A1:A100") ' Definer området der du vil søke
        .Replace What:="gammel", Replacement:="ny", MatchCase:=False, LookAt:=xlPart
    End With
End Sub
```

### Søke og erstatte i Word:
På samme måte har Word en kraftig `Find`- og `Replace`-funksjon tilgjengelig gjennom VBA.

Eksempel:
```vb
Sub ErstattTekstIWord()
    Dim doc As Document
    Set doc = ActiveDocument
    
    With doc.Content.Find
        .Text = "spesifikk"
        .Replacement.Text = "bestemt"
        .Execute Replace:=wdReplaceAll
    End With
End Sub
```

## Dypdykk:
Søking og erstatning av tekst i VBA går tilbake til tidlige automatiseringsfunksjoner i Microsoft Office-applikasjoner, noe som betydelig øker produktiviteten ved å skripte repeterende oppgaver. Over tid har disse funksjonene utviklet seg til å bli kraftigere og mer fleksible, og dekker et bredt spekter av brukstilfeller.

Selv om VBA sin `Replace`-funksjon er praktisk for enkle tekstoperasjoner, gir Excel- og Word-objektmodellene større kontroll og bør brukes for applikasjonsspesifikke oppgaver. De støtter avanserte funksjoner som mønstersøking, bevaring av formatering og nyanserte søkekriterier (f.eks. matche case, hele ord).

Imidlertid, VBA og dets tekstmanipuleringskapasiteter, selv om de er robuste innenfor Microsoft-økosystemet, er kanskje ikke alltid det beste verktøyet for høytytende eller mer komplekse tekstbehandlingsbehov. Språk som Python, med biblioteker som `re` for regulære uttrykk, tilbyr mer kraftfulle og allsidige tekstmanipuleringsalternativer. Men for dem som allerede jobber innenfor Microsoft Office-applikasjoner, forblir VBA et tilgjengelig og effektivt valg for å automatisere søk og erstatt-oppgaver.
