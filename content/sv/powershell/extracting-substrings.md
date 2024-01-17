---
title:                "Utvinna delsträngar"
html_title:           "PowerShell: Utvinna delsträngar"
simple_title:         "Utvinna delsträngar"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Substring-extrahering är processen att extrahera en del av en sträng baserat på ett visst mönster eller plats i strängen. Det är ett användbart verktyg för att manipulera data och hitta specifika bitar av information. Programerare använder detta för att automatiskt extrahera relevanta data från stora mängder information, vilket sparar tid och minskar risken för fel.

## Hur man gör:
För att extrahera en substring i PowerShell, används kommandot `SubString()`. Detta kommando tar två parametrar: startposition och längd på den sträng som ska extraheras. 
```
PowerShell -extractcmd:
$substring = "Detta är en textsträng"
$substring.SubString(5,3)
```
Utskriften av detta kommando skulle vara "är", eftersom det börjar vid position 5 och är 3 tecken långt. 

## Djupdykning:
Substring-extrahering är en vanlig process inom programmering och har funnits sedan de tidiga dagarna av datorer. Det är ett mer effektivt sätt att hitta specifika bitar av information än att manuellt söka igenom en lång sträng. Alternativ till string-extrahering inkluderar användning av reguljära uttryck och kommandon som `Select-String`. Implementationen av SubString() beror på programmeringsspråket men kräver vanligtvis samma typ av parametrar. 

## Se även:
- [Microsofts guide för SubString()](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=bcl-2.0)
- [PowerShell dokumentation för ExtractSubstring()](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/extract-substring?view=powershell-7)