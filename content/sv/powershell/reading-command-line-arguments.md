---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

---

## Vad & Varför?

Kommandoradsargument är värden som sänds till ett program när de körs. Programmerare gör detta för att anpassa programens beteende vid körningstid.

## Så här gör du:

För att läsa in kommandoradsargument i PowerShell använder du `$args` variablen. Till exempel:

```PowerShell
# Skript.ps1
param (
    [string]$namn,
    [string]$ålder
)

Write-Host "Hej $namn! Du är $ålder år gammal."
```

Och om du kör detta skript med argument skulle det se ut så här:

```PowerShell
.\Skript.ps1 -namn "Kalle" -ålder 25
```

Utskriften skulle då bli:

```PowerShell
Hej Kalle! Du är 25 år gammal.
```

## Djupdykning

Kommandoradsargument användas i programmering länge och har sina rötter i UNIX-världen. Även om `$args` är det vanligaste sättet att läsa in argument i PowerShell, kan parametrar också användas för att skapa mer följsamma och användarvänliga skript.

Det är värt att notera att `$args` fungerar lite annorlunda beroende på scriptets kontext. I script och funktioner representerar `$args` alla icke-namngivna parametrar som inte matchas till en parameterdeklaration. I en fil som inte har någon parametrar deklarerade representerar `$args` alla argument som överförs till filen.

## Se även

- [Om_Functions_Advanced_Parameters](https://docs.microsoft.com/sv-se/powershell/scripting/learn/deep-dives/everything-about-param?view=powershell-7.1)
- [Om_Arguments](https://docs.microsoft.com/sv-se/powershell/module/microsoft.powershell.core/about/about_argumentCompleters?view=powershell-7.1)