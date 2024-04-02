---
date: 2024-01-20 17:51:35.701221-07:00
description: "Interpolering av str\xE4ngar handlar om att stoppa in variabler eller\
  \ uttryck direkt i en textstr\xE4ng. Det sparas tid och \xF6kar l\xE4sbarheten i\
  \ din kod genom att\u2026"
lastmod: '2024-03-13T22:44:38.110335-06:00'
model: gpt-4-1106-preview
summary: "Interpolering av str\xE4ngar handlar om att stoppa in variabler eller uttryck\
  \ direkt i en textstr\xE4ng. Det sparas tid och \xF6kar l\xE4sbarheten i din kod\
  \ genom att\u2026"
title: "Interpolera en str\xE4ng"
weight: 8
---

## Vad & Varför?
Interpolering av strängar handlar om att stoppa in variabler eller uttryck direkt i en textsträng. Det sparas tid och ökar läsbarheten i din kod genom att slippa klumpig konkaterning.

## Hur man gör:
```PowerShell
$name = 'Viking'
$age = 782
# Använda stränginterpolering med dubbla citattecken
greeting = "Hej, jag heter $name och jag är $age år gammal."
Write-Host $greeting

# Exempel med subexpression
$doubleAge = "Dubbelt så gammal skulle vara $(2 * $age) år."
Write-Host $doubleAge
```
Output:
```
Hej, jag heter Viking och jag är 782 år gammal.
Dubbelt så gammal skulle vara 1564 år.
```
## Djupdykning
I PowerShell är interpolering av strängar en del av språket sedan den första versionen. Alternativ till interpolering är att använda plus-tecken (+) för att lägga ihop strängar eller -f operatorn för att forma strängar, men dessa metoder kan bli röriga. Interpolering görs genom att packa in variabler eller uttryck i `$()` inuti en sträng omsluten av dubbla citattecken, som tolkar innehållet och konverterar det till en sträng.

## Se också
- Microsofts officiella dokumentation om stränginterpolering: [about_Quoting_Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Quoting_Rules)
