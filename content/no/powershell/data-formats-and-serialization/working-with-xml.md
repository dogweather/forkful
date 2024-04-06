---
date: 2024-01-26 04:34:20.036693-07:00
description: "Hvordan: XML, eller det utvidbare merkespr\xE5ket, har eksistert siden\
  \ slutten av 90-tallet og forblir et mye brukt format for strukturerte data. PowerShell\u2026"
lastmod: '2024-04-05T22:50:55.046259-06:00'
model: gpt-4-0125-preview
summary: "XML, eller det utvidbare merkespr\xE5ket, har eksistert siden slutten av\
  \ 90-tallet og forblir et mye brukt format for strukturerte data."
title: "\xC5 jobbe med XML"
weight: 40
---

## Hvordan:
```PowerShell
# Laste en XML-fil inn i en variabel
[xml]$xmlInnhold = Get-Content 'sti\til\din\fil.xml'

# Tilgang til XML-noder
$bøker = $xmlInnhold.katalog.bok
foreach ($bok in $bøker) {
  Write-Output "Tittel: $($bok.tittel)"
}

# Opprette et nytt XML-element
$nyBok = $xmlInnhold.CreateElement("bok")
$nyBok.SetAttribute("id", "bk999")
$xmlInnhold.DocumentElement.AppendChild($nyBok)

# Lagre XML tilbake til fil
$xmlInnhold.Save('sti\til\din\oppdaterte\fil.xml')
```
Eksempel på utdata:
```
Tittel: Programmering med PowerShell
Tittel: XML Grunnleggende
```

## Dypdykk
XML, eller det utvidbare merkespråket, har eksistert siden slutten av 90-tallet og forblir et mye brukt format for strukturerte data. PowerShell forenkler arbeid med XML sammenlignet med tradisjonelle tolkningsmetoder; det kaster XML til objekter direkte, noe som lar deg samhandle med elementer gjennom kjent punktnotasjon.

Alternativer til XML inkluderer JSON, YAML eller egendefinerte dataformater. JSON, for eksempel, har fått popularitet for sin lette natur og enkle bruk med webteknologier. Imidlertid gjør XMLs utvidede funksjoner som navneområder, skjemaer og XSLT-behandling det ofte til et bedre valg for komplekse dokumenter eller industristandarder.

PowerShell bruker .NET Frameworks XML-evner for sin XML-håndtering. Dette betyr at det ikke bare handler om enkle lese-skrive-operasjoner; du kan også jobbe med XML-skjemaer for validering, bruke XPath for spørringer og benytte XSLT-transformasjoner, alt gjennom PowerShell.

## Se Også
- [W3Schools XML-opplæring](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
