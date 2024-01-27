---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) permette di separare i messaggi di errore dall'output normale del programma. È un canale utile per comunicare fallimenti e problemi agli utenti e ad altri programmi senza intasare l'output principale (stdout).

## How to:
Per scrivere su stderr in C#, usa `Console.Error.WriteLine()`. Ecco un semplice esempio:

```C#
using System;

class Program
{
    static void Main()
    {
        Console.Error.WriteLine("Questo è un messaggio di errore.");
        Console.WriteLine("Questo è un messaggio normale.");
    }
}
```
Output `stderr` (supponendo che venga reindirizzato o visualizzato separatamente):
```
Questo è un messaggio di errore.
```
Output `stdout`:
```
Questo è un messaggio normale.
```

## Deep Dive
`Console.Error` è stato parte del .NET Framework fin dall'inizio, permettendo una chiara distinzione tra normali output e errori. Un'alternativa è usare `Debug` o `Trace` per messaggi diagnostici durante lo sviluppo. Per l'implementazione, `Console.Error` è un `TextWriter` che si comporta come una stream di output per gli errori, simile a come `Console.Out` gestisce l'output standard.

## See Also
- Documentazione ufficiale Microsoft su `Console.Error`: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- Guida su `TextWriter` per output personalizzati: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.textwriter)
- Panoramica su `Debug` e `Trace` in .NET: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug)
