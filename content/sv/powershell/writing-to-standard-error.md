---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
I PowerShell kan du skriva till standardfelet (stderr) för att hantera felmeddelanden separat från vanlig utmatning (stdout). Det är viktigt för att kunna skilja på normal data och fel, vilket förenklar felsökning och loggning.

## How to:
```PowerShell
# Skicka ett enkelt meddelande till standardfelet
Write-Host "Det här är ett felmeddelande" -ForegroundColor Red 1>&2

# Alternativt använd Write-Error för att generera en felutmatning
Write-Error "Oops, något gick fel"

# Fånga felet i en try-catch-block
try {
    Get-Item "icke_existerande_fil.txt"
} catch {
    $_ | Out-String | Write-Host -ForegroundColor Red 1>&2
}

# Exempelutmatning för Write-Error
Oops, något gick fel
+ CategoryInfo          : NotSpecified: (:) [Write-Error], WriteErrorException
+ FullyQualifiedErrorId : Microsoft.PowerShell.Commands.WriteErrorException
```

## Deep Dive:
Standardfelet introducerades i UNIX och har länge använts för att hantera felmeddelanden. PowerShell, inspirerat av UNIX shell, tillhandahåller detta. `Write-Error` är specifik för PowerShell och skapar ett error record. Alternativt kan operatören `1>&2` omdirigera stdout till stderr; `1` representerar stdout och `2` stderr. Implementationen av stderr i PowerShell är viktig vid skript som används i större automatiseringar, där felmeddelanden behöver hanteras skilt.

## See Also:
- [about_Redirection](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_redirection)
- [Write-Error](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error)
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_try_catch_finally)

Observera att länkarna leder till engelskspråkiga resurser, då Microsofts dokumentation oftast är på engelska.
