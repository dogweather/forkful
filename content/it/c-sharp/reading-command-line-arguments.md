---
title:    "C#: Leggere gli argomenti della riga di comando"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Molte volte nella programmazione è necessario creare un programma che possa essere personalizzato dall'utente. Una delle migliori e più semplici opzioni per farlo è l'utilizzo degli argomenti da riga di comando. Continua a leggere per scoprire come farlo in C#!

## Come fare

Per leggere gli argomenti da riga di comando in C#, è necessario utilizzare la classe `Environment`. All'interno della classe, c'è un array chiamato `GetCommandLineArgs()` che restituisce tutti gli argomenti passati al programma al momento dell'esecuzione. Di seguito un esempio di codice che stampa tutti gli argomenti passati al programma:

```C#
using System;

namespace CommandLineArgs
{
  class Program
  {
    static void Main(string[] args)
    {
      // Loop attraverso tutti gli argomenti e stamparli
      for(int i = 0; i < args.Length; i++)
      {
        Console.WriteLine(args[i]);
      }
    }
  }
}
```

Se si esegue il programma con l'argomento `dotnet run arg1 arg2`, l'output sarà:

```
arg1
arg2
```

Si noti che il primo argomento è sempre il percorso del programma stesso.

## Approfondimento

Esistono vari modi per gestire e utilizzare gli argomenti da riga di comando. Ad esempio, è possibile utilizzare librerie esterne come `CommandLineParser` per semplificare la gestione degli input e per impostare argomenti opzionali. Inoltre, è possibile utilizzare gli argomenti da riga di comando per specificare file di input o output per i programmi o per passare informazioni sensibili come password. Esplora e sperimenta con varie soluzioni per vedere quali funzionano meglio per te e per i tuoi progetti.

## Vedi anche

- [Documentazione ufficiale di Microsoft su Environment.GetCommandLineArgs Method](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=net-5.0)
- [Libreria CommandLineParser su GitHub](https://github.com/commandlineparser/commandline)
- [Esempi di progetti con argomenti da riga di comando su GitHub](https://github.com/topics/command-line-arguments)