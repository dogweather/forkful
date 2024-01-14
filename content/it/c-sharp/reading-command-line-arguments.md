---
title:                "C#: Lettura degli argomenti della linea di comando"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché leggere gli argomenti della riga di comando?

Leggere gli argomenti della riga di comando è un'abilità fondamentale per molti programmatori C#. Questa abilità consente di rendere i tuoi programmi più dinamici e personalizzabili in base alle esigenze degli utenti. Inoltre, l'avere familiarità con la lettura degli argomenti della riga di comando può semplificare il processo di debugging e testing dei tuoi programmi.

## Come leggere gli argomenti della riga di comando in C#

Ecco un esempio di codice su come leggere gli argomenti della riga di comando utilizzando la classe "Environment" in C#:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        // stampa il numero totale di argomenti passati alla riga di comando
        Console.WriteLine("Numero di argomenti: {0}", args.Length);

        // itera attraverso ogni argomento e stampa il suo valore
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine("Argomento {0}: {1}", i + 1, args[i]);
        }
    }
}
```

Esempio di output quando si esegue il programma con gli argomenti "hello" e "world":

```bash
> C# CommandLineArguments.exe hello world
Numero di argomenti: 2
Argomento 1: hello
Argomento 2: world
```

È importante notare che gli argomenti della riga di comando verranno passati al programma in forma di stringhe. Questo significa che dovrai conversione a un tipo di dati specifico se desideri utilizzare l'argomento in operazioni matematiche o di altro tipo.

## Approfondimenti sulla lettura degli argomenti della riga di comando

Oltre all'utilizzo della classe "Environment" come mostrato nell'esempio precedente, esiste un altro modo per leggere gli argomenti della riga di comando in C#. Puoi utilizzare la classe "CommandLine" nella libreria "System.CommandLine" per gestire in modo più efficiente gli argomenti passati alla riga di comando. Questa classe ti offre funzionalità come la possibilità di specificare opzioni e parametri richiesti, nonché la possibilità di definire delle descrizioni per gli argomenti.

Per approfondire ulteriormente sulla lettura degli argomenti della riga di comando in C#, puoi consultare la documentazione ufficiale di Microsoft e provare a implementare esempi più complessi utilizzando la classe "CommandLine".

## Vedi anche

- [Documentazione ufficiale di Microsoft sulla lettura degli argomenti della riga di comando in C#](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Esempio di utilizzo della classe "CommandLine" per la lettura degli argomenti della riga di comando in C#](https://www.c-sharpcorner.com/article/command-line-argument-parser-using-system-commandline-in-net-core/)