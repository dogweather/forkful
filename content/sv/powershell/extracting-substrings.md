---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Hämtning av delföljder innebär att man tar en del av en sträng och använder den för ett specifikt syfte. Programmörer gör det för att behandla specifika data från en större datamängd, för exempelvis att analysera, manipulera eller jämföra information.

## Hur gör man:
PowerShell erbjuder olika sätt att extrahera delsträngar. Här är några exempel:

```PowerShell
# Initial sträng
$text = 'Hej, världen!'

# Exempel 1: Extrahera de första 3 tecknen
$substring = $text.Substring(0, 3)
echo $substring # Output: 'Hej'
```

```PowerShell
# Exempel 2: Extrahera delsträng efter en specifik position
$substring = $text.Substring(4)
echo $substring # Output: ', världen!'
```

```PowerShell
# Exempel 3: Använda -split operatören för att extrahera delsträng mellan två specifika tecken
$substring = ($text -split '[,]')[1]
echo $substring # Output: ' världen!'
```

## Djup dykning
Även om .Substring() metoden är en utmärkt väg att extrahera delsträngar, kommer dess ursprung från .NET, vilket gör det mindre användbart i andra programmeringsspråk. Som alternativ kan man använda reguljära uttryck för att få mer flexibilitet, men till priset av läsbarheten.

Implementeringsdetaljer kan variera mellan olika språk. I PowerShell är indexeringen baserad på 0, vilket innebär att det första tecknet i strängen är på index 0.

## Se också
Vill du lära dig mer? Kolla in dessa länkar!
- [Microsoft Docs: String.Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [StackOverflow: Extracting Substrings in PowerShell](https://stackoverflow.com/questions/2108727/substring-extraction-in-powershell)