---
title:                "Hitta längden på en sträng"
html_title:           "PowerShell: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att hitta längden på en sträng är en vanlig uppgift för programmerare. Det innebär helt enkelt att ta reda på hur många tecken en sträng innehåller. Det kan vara användbart för att hantera inmatade data eller för att utföra vissa operationer på en sträng.

# Hur man gör:
```PowerShell
$string = "Hej, jag heter Emma"
$string.Length
```
Detta kommer att returnera längden på strängen "Hej, jag heter Emma", vilket är 19 tecken. Det finns även en inbyggd cmdlet "Get-Length" som kan användas för att hitta längden på en sträng: 

```PowerShell
Get-Length $string
```
Detta kommer också att returnera 19 som längden på strängen.

# Utforska djupare:
Att hitta längden på en sträng är en grundläggande funktion inom programmering och har funnits sedan de första programmeringsspråken skapades. Det finns olika sätt att hitta längden på en sträng, beroende på vilket språk som används. Till exempel kan du använda "len" i Python eller ".length" i JavaScript för att hitta längden på en sträng. Det finns också andra mer avancerade sätt att hitta längden, som att använda regelbundna uttryck eller loopar för att räkna antalet tecken i en sträng.

# Se även:
- [Microsoft Docs: Get-Length](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/get-length?view=powershell-7.1)
- [W3Schools: String length](https://www.w3schools.com/python/ref_func_len.asp)
- [MDN Web Docs: String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)