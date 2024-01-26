---
title:                "Använda reguljära uttryck"
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönster för att matcha text i strängar. Programmerare använder dem för att söka, validera, ersätta eller dela text effektivt.

## Hur gör man:
Exempel 1: Söka efter en e-postadress
```PowerShell
$string = "Kontakta oss på info@example.com."
$pattern = "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+"
if ($string -match $pattern) { "Hittade en e-postadress: $matches[0]" }
```
Output: Hittade en e-postadress: info@example.com

Exempel 2: Byta ut text
```PowerShell
$ersättningstext = "support@example.se"
$string -replace $pattern, $ersättningstext
```
Output: Kontakta oss på support@example.se

## Fördjupning
Reguljära uttryck härstammar från teoretisk datavetenskap och har implementerats i många programmeringsspråk och verktyg. Alternativ till reguljära uttryck inkluderar inbyggda strängfunktioner eller externa bibliotek. Implementationen i PowerShell är kraftfull och använder .NET:s Regex-klass, vilket gör den kompatibel med andra .NET-språk.

## Se även:
- [Regular-Expressions.info](https://www.regular-expressions.info/powershell.html)
- [.NET Docs: Regular Expression Language - Quick Reference](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
