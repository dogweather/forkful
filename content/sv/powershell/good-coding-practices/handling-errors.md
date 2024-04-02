---
date: 2024-01-26 00:56:01.079417-07:00
description: "Att hantera fel i PowerShell inneb\xE4r att f\xF6rutse miss\xF6den och\
  \ hantera dem smidigt. Programmerare g\xF6r detta f\xF6r att f\xF6rhindra krascher\
  \ och f\xF6rse anv\xE4ndare\u2026"
lastmod: '2024-03-13T22:44:38.133271-06:00'
model: gpt-4-1106-preview
summary: "Att hantera fel i PowerShell inneb\xE4r att f\xF6rutse miss\xF6den och hantera\
  \ dem smidigt. Programmerare g\xF6r detta f\xF6r att f\xF6rhindra krascher och f\xF6\
  rse anv\xE4ndare\u2026"
title: Hantering av fel
weight: 16
---

## Vad & Varför?
Att hantera fel i PowerShell innebär att förutse missöden och hantera dem smidigt. Programmerare gör detta för att förhindra krascher och förse användare med användbar återkoppling.

## Hur man gör:
```PowerShell
# Grundläggande Try-Catch för att hantera undantag
try {
    # Kod som kan utlösa ett fel
    $resultat = 1 / 0
} catch {
    # Vad man ska göra om ett fel inträffade
    Write-Host "Hoppsan, ett fel inträffade: $_"
}

# Att skicka ut ett anpassat felmeddelande
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "Filen kunde inte hittas."
}

# Använda $Error-variabeln för att inspektera det senaste felet
```
## Djupdykning
PowerShell har kommit långt sedan dess början som Monad. Felhanteringen har blivit robustare över tiden, och erbjuder funktioner liknande andra programmeringsspråk. Syntaxen `try-catch-finally` är ett sådant exempel på korsbefruktning från språk som C#. Innan dess förlitade sig skriptare starkt på att kontrollera villkor och använda den automatiska variabeln `$Error`.

PowerShell har också två huvudtyper av fel: avslutande och icke-avslutande. Avslutande fel stoppar skriptet om de inte fångas i en `try-catch`-block, medan icke-avslutande fel inte gör det såvida du inte specificerar `-ErrorAction Stop`. Denna åtskillnad är avgörande eftersom den ger fin kontroll över felhantering, och avgör om ett fel verkligen motiverar att stoppa hela skriptet eller kan loggas och ignoreras.

PowerShells felhantering tillåter också ett `finally`-block, som körs oavsett vad - oavsett om ett fel inträffade eller inte. Det är utmärkt för uppröjningsuppgifter.

När du är djupt nere i skriptgraven kan du också hantera specifika undantagstyper, vilket ger dig ännu finare kontroll.

Alternativt finns det den gammaldags parametern `-ErrorVariable` för att fånga fel utan att kasta ett undantag. Och variabeln `$?` berättar för dig om den senaste operationen var framgångsrik. De är användbara verktyg, om än lite mindre rena än en solid `try-catch`.

## Se även
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
