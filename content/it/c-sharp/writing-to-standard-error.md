---
title:                "Scrivere sull'errore standard"
aliases:
- it/c-sharp/writing-to-standard-error.md
date:                  2024-02-03T19:32:43.834118-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere sull'errore standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere su standard error (stderr) in C# implica dirigere i messaggi di errore e le diagnostiche separatamente dall'output regolare (stdout) per aiutare utenti e sviluppatori a distinguere tra l'output normale del programma e le notifiche di errore. I programmatori fanno ciò per rendere il debug e la registrazione più efficienti, consentendo un'operazione e manutenzione delle applicazioni più scorrevoli.

## Come fare:
In C#, scrivere su standard error può essere ottenuto utilizzando il flusso `Console.Error`. Questo flusso è usato specificamente per messaggi di errore e diagnostiche. Ecco un esempio base:

```csharp
Console.Error.WriteLine("Errore: Impossibile elaborare la richiesta.");
```

Output di esempio (su stderr):
```
Errore: Impossibile elaborare la richiesta.
```

Per scenari in cui potresti utilizzare una libreria di terze parti che offre capacità di registrazione avanzate, come `Serilog` o `NLog`, puoi configurare queste librerie per scrivere i log degli errori su stderr. Mentre questi esempi si concentrano sulla semplice reindirizzamento della console, ricorda che in applicazioni in produzione, i framework di registrazione offrono opzioni di gestione degli errori e di output molto più robuste. Ecco un semplice esempio con `Serilog`:

Prima, installa il pacchetto Serilog e il suo sink per la Console:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

Poi, configura Serilog per scrivere su stderr:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("Questo è un messaggio normale.");
Log.Error("Questo è un messaggio di errore.");
```

Output di esempio (su stderr per il messaggio di errore):
```
[15:04:20 ERR] Questo è un messaggio di errore.
```

Nota: La configurazione `standardErrorFromLevel` nel sink console di Serilog reindirizza tutti gli eventi di log al livello specificato (Errore, in questo caso) o superiore verso il flusso di errore standard, mentre messaggi di livello inferiore come Informazioni vengono scritti sul flusso di output standard.
