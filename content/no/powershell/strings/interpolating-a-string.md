---
date: 2024-01-20 17:51:40.752747-07:00
description: "Hvordan: Stringinterpolering, ogs\xE5 kjent som variabelsubstitusjon,\
  \ har v\xE6rt en del av programmeringsspr\xE5k lenge. I PowerShell bruker vi `\"\
  ` (dobbel\u2026"
lastmod: '2024-04-05T21:53:41.963651-06:00'
model: gpt-4-1106-preview
summary: "Stringinterpolering, ogs\xE5 kjent som variabelsubstitusjon, har v\xE6rt\
  \ en del av programmeringsspr\xE5k lenge."
title: Interpolering av en streng
weight: 8
---

## Hvordan:
```PowerShell
$name = 'Viking'
$age = 32

# Interpolere med dobbelt anførselstegn
$message = "Hei, jeg heter $name og jeg er $age år gammel."
Write-Output $message

# Output: Hei, jeg heter Viking og jeg er 32 år gammel.

# Interpolere med subexpression "${}"
$greeting = "Hei, ${name}en!"
Write-Output $greeting

# Output: Hei, Vikingen!
```

## Dypdykk
Stringinterpolering, også kjent som variabelsubstitusjon, har vært en del av programmeringsspråk lenge. I PowerShell bruker vi `"` (dobbel anførselstegn) for å oppnå dette. Historisk sett var vi nødt til å bruke plussoperatoren (+) for å legge sammen strenger og variabler, som kunne bli ganske rotete og vanskelig å lese.

Alternativer til interpolering i PowerShell inkluderer bruk av `-f` operatøren (format operator) eller funksjonen `String.Format()`, som begge tillater mer detaljert formatering av strenger.

Interpolering forenkler skriving av koden og gjør den mer lesbar. Det gjør at koden automatisk konverterer variabelverdier til tekststrengen, uten ekstra arbeid fra programmereren.

Det er viktig å huske at i PowerShell fungerer kun stringinterpolering med dobbelt anførselstegn. Enkelt anførselstegn blir tolket som en literal streng, og eventuelle variabler blir ikke evaluert.

## Se Også
- [PowerShell Dokumentasjon](https://docs.microsoft.com/powershell/)
- [Digital kurs i PowerShell på Microsoft Learn](https://docs.microsoft.com/learn/paths/powershell/)
