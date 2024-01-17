---
title:                "Att göra en sträng stor bokstav"
html_title:           "PowerShell: Att göra en sträng stor bokstav"
simple_title:         "Att göra en sträng stor bokstav"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att "capitaliza" en sträng betyder helt enkelt att ändra den första bokstaven till en stor bokstav och behålla resten av strängen som den är. Detta är vanligtvis gjort av programmörer för att göra utskrifter och visuella presentationer av sina koder mer lättlästa och organiserade.

# Hur man gör:
```PowerShell
$sträng = "det här är en sträng som behöver ett stort begynnelsebokstav"

# Metod 1: Använda .NET-metoden
$sträng = $sträng.Substring(0,1).ToUpper() + $sträng.Substring(1)

# Metod 2: Använda inbyggd PowerShell-funktion
$sträng = $sträng -replace "^.",{$sträng[0].ToString().ToUpper()}

echo $sträng

# Output: Det här är en sträng som behöver ett stort begynnelsebokstav
```

# Djupdykning:
Historiskt sett har "capitaliza" eller "title case" använts för att förbättra läsbarheten och formateringen av text, särskilt i tryckta material. I programmering används detta koncept för att göra kod mer lättläst och strukturerad. Det finns också alternativ som att helt konvertera en sträng till versala bokstäver eller små bokstäver. I PowerShell kan detta åstadkommas med inbyggda metoder som .ToUpper() eller .ToLower().

# Se även:
För mer information om att manipulera strängar i PowerShell, se dessa länkar:
- [PowerShell String manipulation](https://blog.netwrix.com/2018/01/03/how-to-manipulate-strings-in-powershell/)
- [Effective PowerShell: 48 Ways to Capitalize Your String with PowerShell](https://blogs.technet.microsoft.com/heyscriptingguy/2014/01/16/effective-powershell-48-ways-to-capitalize-your-string-with-powershell/)