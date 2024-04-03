---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:17.880777-07:00
description: "Come fare: PowerShell semplifica il processo di scrittura su stderr\
  \ mediante l'uso del cmdlet `Write-Error` o indirizzando l'output al metodo\u2026"
lastmod: '2024-03-13T22:44:43.658751-06:00'
model: gpt-4-0125-preview
summary: PowerShell semplifica il processo di scrittura su stderr mediante l'uso del
  cmdlet `Write-Error` o indirizzando l'output al metodo `$host.ui.WriteErrorLine()`.
title: Scrivere sull'errore standard
weight: 25
---

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
