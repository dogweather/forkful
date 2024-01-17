---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "PowerShell: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att radera karaktärer som matchar ett mönster handlar om att ta bort specifika karaktärer från en sträng eller text baserat på ett fördefinierat mönster. Detta kan vara användbart för att snabbt och effektivt manipulera data och förbättra kodens läsbarhet. Många programmerare använder denna teknik för att rensa upp datafiler eller för att få bort onödiga tecken i en variabel.

# Hur man gör:

```PowerShell
# Skapa en variabel med en sträng
$variabel = "Hej! Det här är en exempelsträng."

# Ta bort alla tecken som matchar mönstret '!'
# Utföra operationen och spara resultatet i en ny variabel
$rensat = $variabel -replace '!'
# Output: Hej Det här är en exempelsträng
```

```PowerShell
# Ta bort alla siffror från en sträng
$sträng = "Det här är strängen 123"
$sträng -replace '\d' # Output: Det här är strängen
```

# Fördjupning:

Att radera karaktärer baserat på ett mönster har funnits sedan tidiga programmeringsspråk som sed och awk. I PowerShell kan kommandot '*replace*' användas för att uppnå detta. Det finns också alternativ som användningen av regular expressions (regex) för mer avancerad mönstermatchning och manipulation.

# Se också:

Officiell dokumentation för '*replace*': <https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/replace?view=powershell-7>

En guide till regular expressions i PowerShell: <https://adamtheautomator.com/regex-powershell/>