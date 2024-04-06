---
date: 2024-01-20 17:51:35.701221-07:00
description: "Hur man g\xF6r: I PowerShell \xE4r interpolering av str\xE4ngar en del\
  \ av spr\xE5ket sedan den f\xF6rsta versionen. Alternativ till interpolering \xE4\
  r att anv\xE4nda plus-\u2026"
lastmod: '2024-04-05T22:50:52.415882-06:00'
model: gpt-4-1106-preview
summary: "I PowerShell \xE4r interpolering av str\xE4ngar en del av spr\xE5ket sedan\
  \ den f\xF6rsta versionen."
title: "Interpolera en str\xE4ng"
weight: 8
---

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
