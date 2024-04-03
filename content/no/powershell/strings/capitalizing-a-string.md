---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:04.268461-07:00
description: "\xC5 sette stor forbokstav i en streng i PowerShell inneb\xE6rer \xE5\
  \ transformere det f\xF8rste tegnet i en gitt streng til stor bokstav mens resten\
  \ av strengen\u2026"
lastmod: '2024-03-13T22:44:41.000004-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sette stor forbokstav i en streng i PowerShell inneb\xE6rer \xE5 transformere\
  \ det f\xF8rste tegnet i en gitt streng til stor bokstav mens resten av strengen\
  \ forblir uendret."
title: Sette stor bokstav i en streng
weight: 2
---

## Hva og hvorfor?
Å sette stor forbokstav i en streng i PowerShell innebærer å transformere det første tegnet i en gitt streng til stor bokstav mens resten av strengen forblir uendret. Programmerere utfører ofte denne oppgaven for formateringsformål, slik som å forberede tekst for visning i brukergrensesnitt eller å følge grammatikalske regler i genererte dokumenter.

## Hvordan:
PowerShell, som er et allsidig verktøy, lar deg sette stor forbokstav i en streng ved hjelp av enkle metoder uten behov for tredjepartsbiblioteker. Slik kan du gjøre det:

```powershell
# Ved å bruke den innebygde .Net-metoden 'ToTitleCase' fra CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
Utdata:
```
Hello world
```

Merk: Denne metoden gjør den første bokstaven i hvert ord til en stor bokstav. Hvis du strengt tatt kun ønsker å sette stor forbokstav på det første tegnet i strengen og la resten være som det er, kan du gjøre noe som dette:

```powershell
# Setter kun stor forbokstav på det første tegnet i en streng
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
Utdata:
```
Hello world
```

PowerShell inkluderer ikke direkte en enkel funksjon for kun å sette stor forbokstav på det første bokstaven i en streng, men ved å kombinere grunnleggende strengmanipuleringsmetoder som `Substring(0,1).ToUpper()` og sammenkjeding, kan vi enkelt oppnå ønsket resultat.
