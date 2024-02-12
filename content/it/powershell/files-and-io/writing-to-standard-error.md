---
title:                "Scrivere sull'errore standard"
aliases: - /it/powershell/writing-to-standard-error.md
date:                  2024-02-03T19:34:17.880777-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere sull'errore standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere su standard error (stderr) in PowerShell implica l'invio di messaggi di errore o diagnostici direttamente allo stream stderr, distinto dallo stream di output standard (stdout). Questa separazione consente un controllo più preciso sull'output di uno script, abilitando gli sviluppatori a dirigere messaggi normali ed errori verso destinazioni diverse, il che è fondamentale per la gestione degli errori e il logging.

## Come fare:

PowerShell semplifica il processo di scrittura su stderr mediante l'uso del cmdlet `Write-Error` o indirizzando l'output al metodo `$host.ui.WriteErrorLine()`. Tuttavia, per la reindirizzazione diretta di stderr, potresti preferire l'uso di metodi .NET o la reindirizzazione dei descrittori di file offerta da PowerShell stesso.

**Esempio 1:** Utilizzo di `Write-Error` per scrivere un messaggio di errore su stderr.

```powershell
Write-Error "Questo è un messaggio di errore."
```

Output su stderr:
```
Write-Error: Questo è un messaggio di errore.
```

**Esempio 2:** Utilizzo di `$host.ui.WriteErrorLine()` per una scrittura diretta su stderr.

```powershell
$host.ui.WriteErrorLine("Scrittura diretta su stderr.")
```

Output su stderr:
```
Scrittura diretta su stderr.
```

**Esempio 3:** Utilizzo di metodi .NET per scrivere su stderr.

```powershell
[Console]::Error.WriteLine("Utilizzo di metodo .NET per stderr")
```

Output di questo metodo:
```
Utilizzo di metodo .NET per stderr
```

**Esempio 4:** Reindirizzamento dell'output di errore usando il descrittore di file `2>`.

I descrittori di file in PowerShell possono reindirizzare diversi stream. Per stderr, il descrittore di file è `2`. Ecco un esempio di reindirizzamento di stderr su un file chiamato `error.log` mentre si esegue un comando che genera un errore.

```powershell
Get-Item FileInesistente.txt 2> error.log
```

Questo esempio non produce output sulla console, ma genera un file `error.log` nella directory corrente contenente il messaggio di errore dal tentativo di accedere a un file che non esiste.

In conclusione, PowerShell fornisce molteplici metodi per scrivere e gestire efficacemente l'output di errore, consentendo strategie sofisticate di gestione degli errori e logging in script e applicazioni.
