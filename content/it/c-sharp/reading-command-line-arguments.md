---
title:                "Lettura degli argomenti da linea di comando"
html_title:           "C#: Lettura degli argomenti da linea di comando"
simple_title:         "Lettura degli argomenti da linea di comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Che cos'è e perché leggere gli argomenti della riga di comando?

Leggere gli argomenti della riga di comando è il processo di acquisire dati o input da parte dell'utente attraverso la riga di comando di un'applicazione. Questa pratica è comune tra i programmatori in quanto consente di passare informazioni o opzioni al programma in esecuzione.

Come si fa:

```C#
// Codice di esempio per leggere gli argomenti della riga di comando

static void Main(string[] args)
{
    // args è una matrice di stringhe che contiene gli argomenti della riga di comando
    // Puoi utilizzare il metodo Length per determinare il numero di argomenti
    Console.WriteLine("Hai fornito " + args.Length + " argomenti:");

    // Stampiamo gli argomenti uno per uno
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine("- " + args[i]);
    }
}

// Output di esempio
// > dotnet myprogramma.cs arg1 arg2 arg3
// Hai fornito 3 argomenti:
// - arg1
// - arg2
// - arg3

```

Approfondimenti:

Leggere gli argomenti della riga di comando ha una lunga storia nell'ambito della programmazione, risalente ai primi tempi dei sistemi operativi a riga di comando come MS-DOS. Prima dell'avvento delle interfacce grafiche, questo era il principale modo per interagire con il computer.

C'è anche un'alternativa a utilizzare gli argomenti della riga di comando: leggere input da tastiera. Questo metodo richiede all'utente di inserire dati durante l'esecuzione del programma. Tuttavia, l'utilizzo degli argomenti della riga di comando è preferibile in quanto rende il processo più veloce e preciso, in quanto l'utente può fornire tutti i dati necessari all'avvio del programma.

Per quanto riguarda l'implementazione, leggere gli argomenti della riga di comando in C# è un processo semplice grazie alla classe "System.Environment". Questa classe contiene il metodo "GetCommandLineArgs()" che restituisce una matrice di stringhe contenente tutti gli argomenti della riga di comando.

Vedi anche:

- Microsoft Docs su "System.Environment Class": https://docs.microsoft.com/en-us/dotnet/api/system.environment?view=net-5.0
- Guida completa a "Command-Line Arguments" in C#: https://www.c-sharpcorner.com/UploadFile/TechSrijit/Command-LineForCSharpTS11232005021730AM/Command-LineForCSharpTS.aspx