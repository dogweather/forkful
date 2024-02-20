---
date: 2024-01-26 04:17:04.412781-07:00
description: "La shell interattiva, o Ciclo Leggi-Valuta-Stampa (REPL, dall'inglese\
  \ Read-Eval-Print Loop), ti consente di digitare comandi PowerShell e ottenere un\u2026"
lastmod: 2024-02-19 22:05:02.718371
model: gpt-4-0125-preview
summary: "La shell interattiva, o Ciclo Leggi-Valuta-Stampa (REPL, dall'inglese Read-Eval-Print\
  \ Loop), ti consente di digitare comandi PowerShell e ottenere un\u2026"
title: Utilizzo di un interprete interattivo (REPL)
---

{{< edit_this_page >}}

## Cosa & Perché?
La shell interattiva, o Ciclo Leggi-Valuta-Stampa (REPL, dall'inglese Read-Eval-Print Loop), ti consente di digitare comandi PowerShell e ottenere un feedback immediato. I programmatori la utilizzano per testare rapidamente frammenti di codice, per il debug o per imparare nuovi comandi senza dover scrivere uno script completo.

## Come fare:
Avvia PowerShell e ti troverai nella REPL. Prova il Cmdlet `Get-Date`:

```PowerShell
PS > Get-Date
```

Dovresti vedere l'output della data e dell'ora correnti:

```PowerShell
Mercoledì, 31 Marzo 2023 12:34:56 PM
```

Ora, concatena i comandi. Proviamo a ordinare i processi per uso della memoria:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Questo comando mostra i primi 5 processi per dimensione del set di lavoro (uso della memoria).

## Approfondimento
La REPL di PowerShell ha le sue radici nella shell Unix e in altre shell di linguaggi dinamici come quella di Python. Si tratta di un ambiente di esecuzione comandi interattivo monoutente. A differenza di un linguaggio compilato in cui si scrivono applicazioni complete e poi si compila, un ambiente REPL permette di scrivere ed eseguire codice una riga alla volta. PowerShell supporta anche l'esecuzione di script per compiti più ampi.

Alternative per Windows includono il Prompt dei Comandi o altre REPL specifiche per linguaggio come IPython. Nel mondo Unix/Linux, shell come bash o zsh svolgono una funzione simile.

L'implementazione di PowerShell utilizza un'applicazione host per eseguire la shell. Anche se PowerShell.exe in Windows è la più comune, altre come l'Ambiente di Scripting Integrato (ISE) o il terminale integrato di Visual Studio Code possono fungere da host.

## Vedi anche
- [Informazioni su PowerShell](https://docs.microsoft.com/it-it/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
