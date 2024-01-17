---
title:                "Sammanfogande av strängar"
html_title:           "PowerShell: Sammanfogande av strängar"
simple_title:         "Sammanfogande av strängar"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & varför?
Konkatenering av strängar är en vanlig programmeringspraxis där flera strängar kombineras till en enda sträng. Det kan vara användbart för att skapa längre strängar eller dynamiskt generera textbaserade data.

## Så här gör man:
```PowerShell
$förnamn = "Lisa"
$efternamn = "Svensson"

$sträng = $förnamn + " " + $efternamn 
# Resultat: Lisa Svensson
```
En annan metod är att använda inbyggda strängfunktioner för att konkatenera strängar, till exempel ```$sträng = [string]::Concat($förnamn, $efternamn)```

## Djupdykning:
Konkatenering av strängar har funnits sedan de tidiga dagarna av programutveckling och är en universell funktion i de flesta programmeringsspråk. Det finns även andra sätt att konkatenera strängar såsom interpolering, där variabler kan infogas direkt i en sträng, till exempel ```"Mitt namn är $förnamn"```

## Se även:
Läs mer om strängmanipulering i PowerShell: https://docs.microsoft.com/sv-se/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-6.2.0

För en guide till användning av inbyggda strängfunktioner: https://docs.microsoft.com/sv-se/powershell/module/microsoft.powershell.core/about/about_string_methods?view=powershell-6.2.0