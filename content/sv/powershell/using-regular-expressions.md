---
title:                "Användning av reguljära uttryck"
html_title:           "PowerShell: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att använda reguljära uttryck (även kallat regex) i PowerShell är ett effektivt sätt att manipulera textsträngar. Genom att använda speciella mönster och symboler kan man söka, ersätta och bearbeta text på ett mer avancerat sätt än med vanliga sökfunktioner. Reguljära uttryck är ett kraftfullt verktyg för programmerare som vill effektivisera sin kodning och arbeta mer precist med textdata.

# Hur man gör:
Här är ett exempel på hur man söker efter en viss del av en textsträng och ersätter den med en annan del, genom att använda reguljära uttryck i PowerShell:

```PowerShell
$text = "Hej! Mitt namn är Johan och jag är en PowerShell-nörd."
$text -replace "Johan", "Lisa"
```

Koden ovan kommer att ersätta delen "Johan" med "Lisa" i textsträngen och resulterar i: "Hej! Mitt namn är Lisa och jag är en PowerShell-nörd."

Här är ett annat exempel som visar hur man kan söka efter ett visst mönster och bara behålla de matcher som stämmer överens med detta mönster:

```PowerShell
$text = "Jag är #25 år gammal och mitt körkort är giltigt till 2025."
$text -replace "\d+","#"
```

Koden ovan kommer att söka efter alla numeriska värden i textsträngen och ersätta dem med en hashtag. Resultatet blir: "Jag är # år gammal och mitt körkort är giltigt till #."

# Djupdykning:
Reguljära uttryck har funnits sedan 1950-talet och används i många olika programmeringsspråk idag, inte bara i PowerShell. Det finns också andra sätt att söka och manipulera text, såsom vanliga sökfunktioner eller substring-metoder. Men med reguljära uttryck kan man ge mer detaljerade instruktioner och få mer exakt kontroll över vilken text som man vill hantera. 
I PowerShell används [regex-metoderna](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions) för att hantera reguljära uttryck. Det finns också flera online resurser som [RegExr](https://regexr.com/) och [Regex101](https://regex101.com/) som kan hjälpa till att testa och förstå reguljära uttryck bättre.

# Se även:
- [Microsofts documenation om reguljära uttryck i PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
- [Online regex-testare Regex101](https://regex101.com/)