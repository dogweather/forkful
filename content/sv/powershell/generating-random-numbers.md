---
title:                "Generera slumpmässiga tal"
html_title:           "PowerShell: Generera slumpmässiga tal"
simple_title:         "Generera slumpmässiga tal"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga nummer är ett sätt för programmerare att skapa en uppsättning nummer som inte följer ett specifikt mönster eller regelbundenhet. Det kan användas för att testa program, skapa unika användar-ID eller helt enkelt för underhållning.

## Så här gör du:
```
PowerShell Get-Random -Minimum 1 -Maximum 100
```
Detta kommando genererar ett slumpmässigt heltal mellan 1 och 100. Genom att ändra värden på -Minimum och -Maximum kan man skapa ett intervall som passar ens behov.

```
PowerShell Get-Random -InputObject "röd", "blå", "grön"
```
Genom att använda -InputObject så kan man generera ett slumpmässigt objekt från en lista av värden, i det här fallet olika färger. Det är en användbar funktion när man vill testa olika scenarion eller välja slumpmässiga alternativ.

## Djupdykning:
Generering av slumpmässiga nummer har funnits sedan början av datorer och används i många olika tillämpningar idag. En alternativ metod för att generera slumpmässiga nummer är att använda en särskild hårdvaruenhet som kallas för "random number generator" (RNG). Denna enhet använder fysiska fenomen som inte är helt förutsägbara för att skapa slumpmässiga nummer. Det finns också olika algoritmer för att skapa slumpmässiga nummer, men det rekommenderas att använda inbyggda funktioner i programmeringsspråket istället för att skriva egna.

## Se även:
Läs mer om [PowerShell cmdlet Get-Random](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7) och dess användningsområden.