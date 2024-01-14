---
title:    "C#: Stampa degli output di debug"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è essenziale per identificare e risolvere errori nei tuoi programmi. Senza una stampa di debug efficace, sarebbe molto più difficile individuare e risolvere i problemi nel codice.

## Come fare

Per stampare un'output di debug in C#, puoi utilizzare il metodo `Debug.WriteLine()`. Ecco un esempio di codice che stamperà un messaggio di debug e il valore di una variabile:

```C#
int numero = 10;
Debug.WriteLine("Il numero è: " + numero);
```

L'output di questo codice sarebbe `Il numero è: 10` nella finestra di output del tuo ambiente di sviluppo.

## Deep Dive

Per prima cosa, è importante considerare quali messaggi di debug sono veramente necessari per risolvere i problemi del tuo programma. L'inserimento di troppi messaggi di debug può appesantire il codice e rallentare l'esecuzione del programma.

Inoltre, puoi utilizzare la classe `Trace` per una stampa di debug più avanzata. Questa classe fornisce metodi per generare output di debug di diversi livelli, aiutandoti a controllare meglio le informazioni che vengono stampate.

## Vedi anche

- [Documentazione ufficiale di Debug.WriteLine()](https://docs.microsoft.com/it-it/dotnet/api/system.diagnostics.debug.writeline)
- [Documentazione ufficiale di Trace](https://docs.microsoft.com/it-it/dotnet/api/system.diagnostics.trace)