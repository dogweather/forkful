---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:58:34.396588-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Att kolla om en mapp finns är processen där man verifierar om en specifik mapp existerar i filsystemet. Programmerare gör detta för att undvika fel när man försöker åtkomma eller manipulera mappar som inte finns.

## How to:
Använd `Test-Path` för att snabbt kontrollera mappar. Här är ett exempel:

```PowerShell
# Kolla om en mapp existerar
$mapp = "C:\MinMapp"
if (Test-Path $mapp) {
    Write-Host "Mappen finns."
} else {
    Write-Host "Mappen finns inte."
}
```

Utfall:

```
Mappen finns.
```

Eller:

```
Mappen finns inte.
```

## Deep Dive
I historien har Windows-användare lärt sig att använda `if`-satser och kommandon som `exist` i batch-skript. PowerShell erbjuder nu en mer kraftfull och lättläst metod med `Test-Path`.

Det finns alternativ till `Test-Path`, som att försöka navigera till mappen med `Set-Location` och fånga undantag om det misslyckas, men `Test-Path` är det snabbaste och enklaste sättet.

`Test-Path` kan också användas med olika parametrar för att finjustera vad du kontrollerar, som att särskilja filer från mappar med `-PathType` flaggan:

```PowerShell
# Kolla endast om en mapp existerar, inte en fil
Test-Path $mapp -PathType Container
```

Detaljerna i `Test-Path` implementeringen är inbyggda i PowerShell och använder .NET Framework (eller .NET Core/.NET 5+ i nyare versioner av PowerShell) under ytan, vilket ger god prestanda och tillförlitlighet över olika Windows-versioner såväl som cross-platform.

## See Also
- [Om `Test-Path` i PowerShell-dokumentationen](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Test-Path)
- [Om att hantera undantag i PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-exceptions?view=powershell-7.1)