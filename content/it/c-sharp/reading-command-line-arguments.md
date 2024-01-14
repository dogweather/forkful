---
title:                "C#: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché
Molti programmatori possono essere tentati di saltare la lettura degli argomenti della riga di comando quando scrivono un programma, ma in realtà è un'importante abilità da avere sotto mano. La lettura degli argomenti della riga di comando consente di rendere il programma più flessibile, in quanto gli utenti possono passare facilmente informazioni al programma senza dover modificare il codice sorgente.

## Come
Per leggere gli argomenti della riga di comando in C#, è necessario utilizzare il metodo `Main()` della classe `Program` che si trova nel file di avvio del programma. Utilizzando il parametro `string[] args`, è possibile accedere agli argomenti passati durante l'esecuzione del programma. Esempio di codice:

```C#
static void Main(string[] args)
{
    // Controlla se ci sono almeno 2 argomenti passati
    if (args.Length >= 2)
    {
        // Stampa il primo e il secondo argomento passato
        Console.WriteLine($"Primo argomento: {args[0]}");
        Console.WriteLine($"Secondo argomento: {args[1]}");
    }
}
```

Esempio di output dalla riga di comando:

```
> dotnet myProgram.cs arg1 arg2
Primo argomento: arg1
Secondo argomento: arg2
```

## Deep Dive
La classe `Program` può anche essere configurata per accettare opzioni da riga di comando utilizzando la libreria `Microsoft.Extensions.CommandLineUtils`. Questo consente di definire opzioni e argomenti con valori predefiniti, gestire errori di input e altro ancora. Esempio di codice:

```C#
static int Main(string[] args)
{
    // Configurare la classe CommandLineApplication
    CommandLineApplication commandLineApp = new CommandLineApplication();

    // Definire uno o più opzioni e argomenti
    var option = commandLineApp.Option("-u|--username <USERNAME>", "Nome utente", CommandOptionType.SingleValue);
    var argument = commandLineApp.Argument("password", "Password", multipleValues: true);

    // Opzione predefinita se l'opzione non viene specificata
    option.ShowInHelpText = true;

    // Gestisco gli errori di input
    commandLineApp.OnValidationError((exception) => {
        Console.WriteLine($"Errore: {exception.Message}");
        commandLineApp.ShowHelp();
        Environment.Exit(1);
    });

    // Eseguire l'applicazione dei comandi
    commandLineApp.Execute(args);

    // Utilizzare l'opzione e l'argomento nella logica del programma
    if (!string.IsNullOrEmpty(option.Value()))
    {
        Console.WriteLine($"Username: {option.Value()}");
    }
    if (!argument.Values.Any())
    {
        Console.WriteLine("Password non specificata");
        return 1; // Codice di errore
    }
    foreach (var password in argument.Values)
    {
        Console.WriteLine($"Password: {password}");
    }

    return 0; // Successo
}
```

Esempio di output dalla riga di comando:

```
> dotnet myProgram.cs --username user123 pass1 pass2 pass3
Username: user123
Password: pass1
Password: pass2
Password: pass3
```

## Guarda anche
- [Microsoft Documentation on Command Line Arguments](https://docs.microsoft.com/en-us/dotnet/core/extensions/command-line-args)
- [Example of Command Line Arguments in C#](https://www.c-sharpcorner.com/article/command-line-args-in-c-sharp/)
- [Handling Command Line Arguments in C#](https://www.meziantou.net/command-line-parsing.htm)