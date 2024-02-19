---
aliases:
- /no/powershell/working-with-xml/
date: 2024-01-26 04:34:20.036693-07:00
description: "\xC5 jobbe med XML inneb\xE6rer \xE5 manipulere og f\xE5 tilgang til\
  \ data strukturert i det utvidbare merkespr\xE5ket (eXtensible Markup Language).\
  \ Programmerere\u2026"
lastmod: 2024-02-18 23:08:54.124094
model: gpt-4-0125-preview
summary: "\xC5 jobbe med XML inneb\xE6rer \xE5 manipulere og f\xE5 tilgang til data\
  \ strukturert i det utvidbare merkespr\xE5ket (eXtensible Markup Language). Programmerere\u2026"
title: "\xC5 jobbe med XML"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med XML innebærer å manipulere og få tilgang til data strukturert i det utvidbare merkespråket (eXtensible Markup Language). Programmerere arbeider med XML for å muliggjøre samspill med andre systemer eller for å lese og skrive konfigurasjonsfiler, datastrømmer og andre strukturerte dokumenter vanlige i webtjenester.

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
