---
title:                "Gestione degli errori"
date:                  2024-01-26T00:56:36.079734-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Gestire gli errori in PowerShell significa prevedere gli imprevisti e gestirli in modo fluido. I programmatori lo fanno per prevenire crash e fornire agli utenti un feedback utile.

## Come fare:
```PowerShell
# Try-Catch di base per gestire le eccezioni
try {
    # Codice che potrebbe innescare un errore
    $result = 1 / 0
} catch {
    # Cosa fare se si verifica un errore
    Write-Host "Ops, si è verificato un errore: $_"
}

# Generazione di un messaggio di errore personalizzato
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "Il file non è stato trovato."
}

# Utilizzo della variabile $Error per ispezionare l'ultimo errore
```
## Approfondimento
PowerShell ha fatto molta strada dalla sua nascita come Monad. La gestione degli errori è diventata più robusta nel tempo, offrendo caratteristiche simili ad altri linguaggi di programmazione. La sintassi `try-catch-finally` è uno di questi elementi portati da linguaggi come C#. Prima, gli scripter si affidavano molto sul controllo delle condizioni e sull'uso della variabile automatica `$Error`.

PowerShell ha anche due tipi principali di errori: terminanti e non terminanti. Gli errori terminanti interromperanno lo script a meno che non vengano catturati in un blocco `try-catch`, mentre quelli non terminanti non lo faranno, a meno che non si specifichi `-ErrorAction Stop`. Questa distinzione è fondamentale poiché conferisce un controllo accurato sulla gestione degli errori, decidendo se un errore giustifica davvero l'arresto di tutto lo script o può semplicemente essere registrato e ignorato.

La gestione degli errori di PowerShell consente l'uso di un blocco `finally`, che si esegue in ogni caso - a prescindere che si sia verificato un errore o meno. È ottimo per operazioni di pulizia.

Quando sei immerso nelle trincee dello scripting, puoi anche gestire tipi specifici di eccezioni, ottenendo un controllo ancora più preciso.

In alternativa, c'è il vecchio parametro `-ErrorVariable` per catturare errori senza generare un'eccezione. E la variabile `$?` ti dice se l'ultima operazione è stata eseguita con successo. Sono strumenti utili, anche se un po' meno puliti di un solido `try-catch`.

## Vedi Anche
- [about_Try_Catch_Finally](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
