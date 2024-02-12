---
title:                "Bruke assosiative tabeller"
aliases:
- /no/vba/using-associative-arrays/
date:                  2024-02-01T22:04:30.332870-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke assosiative tabeller"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/vba/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller, ofte kjent som ordbøker i Visual Basic for Applications (VBA), lar programmerere lage samlinger av nøkkel-verdi-par. Denne funksjonen er avgjørende for effektiv datalagring og gjenfinning, og tilbyr en mer fleksibel og intuitiv måte å håndtere data på enn tradisjonelle arrayindekser.

## Hvordan:

I VBA tilbyr `Dictionary`-objektet funksjonalitet lik assosiative tabeller. Du må først legge til en referanse til Microsoft Scripting Runtime for å bruke den:

1. I VBA-editoren, gå til Verktøy > Referanser...
2. Merk "Microsoft Scripting Runtime" og klikk OK.

Slik deklarerer, fyller og får du tilgang til elementer i en `Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Legge til elementer
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Ingeniør"

' Få tilgang til elementer
Debug.Print sampleDictionary.Item("Name")  ' Utdata: John Doe
Debug.Print sampleDictionary.Item("Age")   ' Utdata: 29

' Sjekke om en nøkkel finnes
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Occupation Key Exists"
End If

' Fjerne elementer
sampleDictionary.Remove("Occupation")

' Løkke gjennom ordboken
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Dypdykk

`Dictionary`-objektet bak kulissene grensesnitt med komponenter i Windows Scripting Host. Som sådan er det et sent bundet COM-objekt, noe som var en vanlig måte å utvide VBA-funksjonaliteten på i fortiden. Bruken av det i VBA kan betydelig forbedre språkets evne til å manipulere komplekse datasett uten å håndheve en fast struktur, som sett i tradisjonelle tabeller eller Excel-områder.

En begrensning å huske på er at tilgang til `Dictionary` krever at man setter en referanse til Microsoft Scripting Runtime, noe som kan komplisere distribusjonen av VBA-prosjektene dine. Alternativer som Collections eksisterer innen VBA, men mangler noen av `Dictionary`-ens nøkkelfunksjoner, som for eksempel evnen til enkelt å sjekke for eksistensen av en nøkkel uten å utløse en feil.

I mer nylige programmeringskontekster tilbyr språk som Python innebygd støtte for assosiative tabeller (også kjent som ordbøker i Python) uten behov for å legge til eksterne referanser. Denne innebygde støtten forenkler prosessen og tilbyr mer avanserte funksjoner ut av boksen. Imidlertid, innenfor rammen av VBA og for spesifikke applikasjoner rettet mot automatisering av oppgaver i Microsoft Office-pakken, forblir bruk av `Dictionary`-objektet en kraftig og relevant metode for datastrukturer som ligner assosiative tabeller.
