---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp existerar är att låta ditt PowerShell skript avgöra om en viss katalog finns eller inte på din dator. Programmerare gör det för att undvika fel som uppstår när skript försöker läsa, skriva, eller navigera till en mapp som inte finns.

## Hur man gör:
Här är PowerShell-skriptet för att kontrollera om en mapp existerar.

```PowerShell
$path = 'C:\Temp'
if (Test-Path $path) {
    Write-Host 'Mappen finns.'
} else {
    Write-Host 'Mappen finns inte.'
}
```

Kodens utdata kommer att vara antingen 'Mappen finns.' om den angivna mappen finns, eller 'Mappen finns inte.' om den inte gör det.

## Djupdykning
Historiskt sett fanns det andra sätt att kontrollera ifall en mapp existerar i äldre versioner av PowerShell, men `Test-Path` är det vanligaste och mest pålitliga sättet i dagens version.

Alternativa sätt att kontrollera om en mapp existerar inkluderar användning av .NET bibliotek och cmdlets som `Get-ChildItem`, men `Test-Path` har fördelen att det är enklare och snabbare eftersom det inte läser innehållet i mappen.

När det gäller implementeringsdetaljer returnerar `Test-Path` helt enkelt sant eller falskt baserat på om sökvägen finns. Det följer symboliska länkar, vilket betyder att om sökvägen är en symbolisk länk till en mapp, kommer `Test-Path` att returnera sant även om länken inte har någon destination.

## Se också
För mer information, här är några användbara länkar:

- [`Test-Path` dokumentation](https://docs.microsoft.com/sv-se/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1)
- [Diskussion om alternativa metoder](https://stackoverflow.com/questions/9149997/different-ways-to-check-if-a-directory-exists-in-powershell)
- [Mer om PowerShell och mappar](https://devblogs.microsoft.com/scripting/weekend-scripter-use-powershell-to-easily-find-system32-folder/)