---
title:                "Scrivere sull'errore standard"
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Scrivere su standard error (stderr) significa inviare messaggi di errore o diagnostici separati dall'output standard. I programmatori lo fanno per isolare gli errori dall'output utile, semplificando il debugging e la gestione degli errori.

## How to: (Come fare:)
Usa `Write-Host` con il parametro `-ForegroundColor`. Aggiungi `>&2` per reindirizzare a stderr.

```PowerShell
# Scrivi un messaggio di errore in rosso
Write-Host "Errore: File non trovato!" -ForegroundColor Red >&2

# Reindirizza un errore da cmdlet a stderr
Get-Item "non_esiste.txt" 2>&1
```

Output d'esempio:
```
Errore: File non trovato!
Get-Item: Impossibile trovare il percorso 'C:\...\non_esiste.txt' perché non esiste.
```

## Deep Dive (Approfondimento)
Historically, i sistemi operativi differenziano fra stdout e stderr per permettere un'elaborazione separata. PowerShell supporta questo standard UNIX. Alternativamente, puoi usare `Write-Error` o `[Console]::Error.WriteLine()` per stderr. L'implementazione si basa sulla redirection interna dei flussi del sistema operativo.

## See Also (Vedi Anche)
- Microsoft Docs su Write-Host: [https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/write-host](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/write-host)
- Redirection in PowerShell: [https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_redirection](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_redirection)
- StackOverflow discussions on stderr: [https://stackoverflow.com/questions/tagged/powershell+stderr](https://stackoverflow.com/questions/tagged/powershell+stderr)
