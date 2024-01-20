---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# PowerShell: Stränginterpolationens kraft

## Vad och Varför?
Stränginterpolation är en teknik för att ersätta platsbevarare i en sträng med deras motsvarande värden. Programmerare använder det för att infoga variabler eller uttryck direkt i en strängtext, det gör koden mer läsbar och underhållbar.

## Hur du gör:
Här är några exempel på hur du använder stränginterpolation i PowerShell:

```PowerShell
$name = "Alice"
echo "Hej, $name"
```
Output: `Hej, Alice`

Vill du göra mer komplexa saker med stränginterpolation? Kolla in följande exempel:

```PowerShell
$hour = (Get-Date).Hour
echo "Klockan är nu: $hour"
```
Output: `Klockan är nu: [Aktuell timme här]` 

## Fördjupning
Powershell stränginterpolation introducerades ursprungligen med unix-skalskript och har sedan dess integrerats i många programmeringsspråk. Ett alternativ till stränginterpolation är att använda sammanfogningsoperatorer eller formateringsfunktioner, men de kan göra koden mindre läsbar. När det kommer till prestanda, tänk på att omfattande stränginterpolation kan sakta ner ditt script, eftersom varje interpolation kräver ytterligare CPU-cykler för att tolka och ersätta platsbevararen.

## Se även
Om du vill lära dig mer om PowerShell stränginterpolation, rekommenderar jag följande resurser: 

- [Microsoft Docs: About String Interpolation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-7.1)
- [Stack Overflow: How to use string interpolation in PowerShell](https://stackoverflow.com/questions/3919755/how-to-format-strings-in-powershell)
- [PowerShell GitHub Repository](https://github.com/PowerShell/PowerShell)