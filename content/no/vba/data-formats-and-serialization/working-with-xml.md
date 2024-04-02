---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:55.728872-07:00
description: "\xC5 arbeide med XML i Visual Basic for Applications (VBA) involverer\
  \ parsing, oppretting og modifisering av XML-dokumenter innenfor konteksten av Microsoft\u2026"
lastmod: '2024-03-13T22:44:40.648298-06:00'
model: gpt-4-0125-preview
summary: "\xC5 arbeide med XML i Visual Basic for Applications (VBA) involverer parsing,\
  \ oppretting og modifisering av XML-dokumenter innenfor konteksten av Microsoft\u2026"
title: Arbeide med XML
weight: 40
---

## Hva & Hvorfor?

Å arbeide med XML i Visual Basic for Applications (VBA) involverer parsing, oppretting og modifisering av XML-dokumenter innenfor konteksten av Microsoft Office-applikasjoner. Programmerere vender seg til denne kapabiliteten for å integrere Office-applikasjoner med webtjenester eller andre datakilder som utsteder XML, noe som letter datautveksling og rapporteringsfunksjonalitet.

## Hvordan:

For å begynne å samhandle med XML, bruker man vanligvis `MSXML2.DOMDocument`-objektet. Dette grensesnittet lar deg laste, parse og navigere XML-dokumenter. Nedenfor er et enkelt eksempel som demonstrerer hvordan å laste en XML-fil, navigere dens struktur, og lese attributter og tekstinnhold.

```basic
' Først, sørg for at du har lagt til referansen til "Microsoft XML, v6.0" via Verktøy -> Referanser
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Path\To\Your\File.xml") ' Last din XML-fil

' Sjekk om XML ble lastet vellykket
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "Feil ved lasting av XML:" & xmlDoc.parseError.reason
Else
    ' Navigere og lese elementer
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath for å finne den første <title> innenfor <book>
    MsgBox book.Text ' Vis teksttittelen
End If
```

I eksempelkoden ovenfor oppretter vi en instans av `MSXML2.DOMDocument60`, laster en XML-fil, og sjekker deretter for feil. Hvis ingen feil er funnet, navigerer vi til en spesifikk node ved bruk av XPath og viser dens tekstinnhold.

## Dypdykk:

Integrasjonen av XML-kapasiteter i VBA går tilbake til tidlig på 2000-tallet når behovet for at Office-applikasjoner skulle interagere med webdata og tjenester begynte å vokse. `MSXML`-biblioteket, eller Microsoft XML Core Services, har utviklet seg over årene, med `MSXML2.DOMDocument60` som en av de siste versjonene anbefalt for bruk på grunn av forbedret ytelse og sikkerhetsfunksjoner.

Selv om de er kraftfulle, anses XML-håndteringskapasiteten til VBA som mindre effektivt og mer tungvint sammenlignet med moderne programmeringsmiljøer som Python's XML.etree eller C#'s LINQ to XML. Den iboende ordrikdommen til VBA og kravet til å legge til og håndtere referanser manuelt kan avskrekke rask utvikling. Videre, med fremveksten av JSON som et lettere datautvekslingsformat, flytter mange programmerere og applikasjoner bort fra XML, med mindre interoperabilitet med arvesystemer eller spesifikke bedriftstjenester nødvendiggjør bruken.

Likevel, for oppgaver som krever parsing eller generering av XML-dokumenter innenfor konteksten av Microsoft Office-automatisering, forblir å utnytte VBAs XML-håndteringsfunksjoner en levedyktig og noen ganger nødvendig tilnærming. Dette skaper en balanse mellom å få tilgang til Office-applikasjonenes rikholdige funksjonssett og de strukturerte datahåndteringskapasitetene som tilbys av XML.
