---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "C#: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se sei nuovo a C# o alla programmazione in generale, potresti chiederti perché dovresti leggere gli argomenti della riga di comando. La risposta è semplice: la lettura degli argomenti della riga di comando ti permette di interagire con il tuo programma in modo diverso, fornendo input personalizzato e rendendo il tuo programma più versatile.

## Come fare

Nel linguaggio C#, ci sono diversi modi per leggere gli argomenti della riga di comando. Il metodo più comune è utilizzare il metodo `Main()` nella tua classe principale. Vediamo un esempio:

```C#
static void Main(string[] args)
{
    // args è un array di stringhe che contiene gli argomenti della riga di comando
    Console.WriteLine("Hai inserito " + args.Length + " argomenti.");

    // cicliamo attraverso l'array per stampare ogni argomento
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine("Argomento " + i + ": " + args[i]);
    }
}
```

Se eseguiamo questo programma da linea di comando, fornendo alcuni argomenti, ad esempio `programma.exe arg1 arg2 arg3`, l'output sarebbe il seguente:

```
Hai inserito 3 argomenti.
Argomento 0: arg1
Argomento 1: arg2
Argomento 2: arg3
```

Un altro modo per leggere gli argomenti della riga di comando è utilizzare la classe `Environment`. Vediamo un esempio:

```C#
string[] args = Environment.GetCommandLineArgs();

// il primo elemento dell'array è il nome del programma, quindi lo saltiamo
for (int i = 1; i < args.Length; i++)
{
    Console.WriteLine("Argomento " + i + ": " + args[i]);
}
```

In questo caso, il risultato sarebbe lo stesso.

## Approfondimento

Oltre ai due modi mostrati sopra, esistono diverse librerie e framework che offrono funzionalità più avanzate per la lettura degli argomenti della riga di comando. Ad esempio, la libreria CommandLineParser consente di specificare argomenti opzionali e obbligatori, assegnare valori a determinati argomenti e generare automaticamente un messaggio di utilizzo per l'utente finale.

Tieni presente che, se vuoi passare argomenti con spazi all'interno, dovrai utilizzare virgolette o backslash per evitare problemi di parsing. Ad esempio:

`programma.exe "primo argomento" "secondo argomento"` oppure `programma.exe primo\ argomento secondo\ argomento`

## Vedi anche

-  [Documentazione ufficiale su Environment](https://docs.microsoft.com/it-it/dotnet/api/system.environment?view=net-5.0)
- [Documentazione ufficiale su CommandLineParser](https://github.com/commandlineparser/commandline)
- [Tutorial di programmazione in C# per principianti](https://docs.microsoft.com/it-it/dotnet/csharp/tutorials/intro-to-csharp/)