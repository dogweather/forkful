---
title:    "C#: Leggere gli argomenti della riga di comando"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore di C#, probabilmente hai già lavorato con variabili e costanti all'interno del tuo codice. Ma cosa succede quando vuoi passare dei dati direttamente da riga di comando? Sapere come leggere gli argomenti della riga di comando può rendere il tuo codice più flessibile e versatile.

## Come fare

Per leggere gli argomenti della riga di comando in C#, dovrai utilizzare la classe `Environment` e il metodo `GetCommandLineArgs()`. Vediamo un esempio:

```C#
static void Main(string[] args)
{
    string[] commandLineArgs = Environment.GetCommandLineArgs();
    foreach (string arg in commandLineArgs)
    {
        Console.WriteLine(arg);
    }
}
```

Nel codice sopra, utilizziamo il metodo `GetCommandLineArgs()` per ottenere un array di stringhe contenente tutti gli argomenti passati in input, incluso il nome del programma. Successivamente, utilizziamo un ciclo `foreach` per stampare ogni argomento sulla console.

Supponiamo che il tuo programma si chiami `programma.exe` e vogliamo passare l'argomento `foo` da riga di comando. L'output del codice sopra sarà il seguente:

```
programma.exe
foo
```

Se vuoi accedere agli argomenti senza includere il nome del programma, puoi semplicemente utilizzare `args[1]` nell'esempio sopra.

## Approfondimento

Oltre a leggere gli argomenti della riga di comando, è possibile anche modificare e gestire gli argomenti prima di utilizzarli nel tuo codice. Ad esempio, puoi aggiungere nuovi argomenti utilizzando il metodo `Environment.SetCommandLineArgs()`.

Inoltre, è importante notare che gli argomenti della riga di comando possono essere sensibili alla maiuscole e minuscole su sistemi operativi diversi. Se vuoi assicurarti che i tuoi argomenti vengano sempre interpretati nello stesso modo, è possibile utilizzare il metodo `String.Compare()` con il parametro `StringComparison.InvariantCultureIgnoreCase`.

## Vedi anche

- [Documentazione di Microsoft su Environment.GetCommandLineArgs()](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs)
- [Tutorial su Channel9 di C# Reading Command Line Arguments](https://channel9.msdn.com/Blogs/Seth-Juarez/C-Reading-Command-Line-Arguments)