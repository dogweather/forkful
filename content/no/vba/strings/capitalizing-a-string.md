---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:14.572989-07:00
description: "\xC5 sette stor bokstav i en streng i Visual Basic for Applications\
  \ (VBA) inneb\xE6rer \xE5 konvertere den f\xF8rste bokstaven i hvert ord i en streng\
  \ til stor\u2026"
lastmod: '2024-02-25T18:49:38.782680-07:00'
model: gpt-4-0125-preview
summary: "\xC5 sette stor bokstav i en streng i Visual Basic for Applications (VBA)\
  \ inneb\xE6rer \xE5 konvertere den f\xF8rste bokstaven i hvert ord i en streng til\
  \ stor\u2026"
title: "\xC5 gj\xF8re om en streng til store bokstaver"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sette stor bokstav i en streng i Visual Basic for Applications (VBA) innebærer å konvertere den første bokstaven i hvert ord i en streng til stor bokstav, mens man sørger for at resten er i små bokstaver. Programmerere gjør dette for normalisering av data, økt lesbarhet, og for å sikre konsistens på tvers av tekstlige datainnganger eller -visninger.

## Hvordan gjøre det:

VBA har ikke en innebygd funksjon spesielt for å sette stor bokstav i hvert ord i en streng, slik noen andre programmeringsspråk har. Du kan imidlertid oppnå dette ved å kombinere noen metoder og funksjoner som `UCase`, `LCase`, og `Mid`.

Her er et enkelt eksempel på hvordan du setter stor bokstav i en streng:

```vb
Function CapitalizeString(inputString As String) As String
    Dim words As Variant
    words = Split(inputString, " ")
    For i = LBound(words) To UBound(words)
        If Len(words(i)) > 0 Then
            words(i) = UCase(Left(words(i), 1)) & LCase(Mid(words(i), 2))
        End If
    Next i
    CapitalizeString = Join(words, " ")
End Function

Sub ExampleUsage()
    Dim exampleString As String
    exampleString = "hello world from VBA!"
    MsgBox CapitalizeString(exampleString) 'Output: "Hello World From Vba!"
End Sub
```

`CapitalizeString`-funksjonen splitter inngangsstrengen inn i ord, setter stor bokstav i første bokstav i hvert ord, og setter dem deretter sammen igjen for å danne den riktig kapitaliserte strengen.

## Dypdykk

Visual Basic for Applications, som dukket opp på begynnelsen av 90-tallet som et makrospråk for Microsoft Office-applikasjoner, ble designet for å tilby en tilgjengelig programmeringsmodell. Dens strengmanipulasjonsmuligheter, selv om de er omfattende, mangler noen høyere nivå abstraksjoner funnet i nyere språk. Mange moderne programmeringsmiljøer tilbyr en dedikert metode for å sette stor bokstav i strenger, ofte omtalt som tittelsaker eller lignende. Python, for eksempel, inkluderer `.title()`-metoden for strenger.

Når man sammenligner, kan fraværet av en enkelt, innebygd funksjon i VBA for å sette stor bokstav i strengord virke som en ulempe. Dette tilbyr imidlertid programmerere en dypere forståelse og kontroll over hvordan de manipulerer tekst og tilrettelegger for nyanser som ikke nødvendigvis følges strengt av en generisk metode. For eksempel, håndtering av akronymer eller spesielle tilfeller der visse mindre ord i titler ikke bør være kapitaliserte, kan bedre tilpasses i VBA gjennom eksplisitte funksjoner.

Videre, mens direkte tilnærminger eksisterer i VBA for å endre saken på en streng (`LCase` og `UCase`), understreker den manuelle ruten for å sette stor bokstav i enkelte ord innenfor en streng den nyanserte kontrollen VBA gir til utviklere. Dette er spesielt viktig i applikasjoner som databasestyring, formsinnganger og dokumentredigering hvor tekstmanipulasjon er hyppig, men variert i krav.

Likevel, for applikasjoner hvor tekstbehandlingskravene er høye og mangfoldige, kan språk med innebygde strengmanipulasjonsbiblioteker tilby en mer effektiv rute. Det er i disse scenariene at integrering eller komplementering av VBA med andre programmeringsressurser, eller å velge et annet språk helt, kunne vise seg å være fordelaktig.
