---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Argomenti da riga di comando in C#

**## Che Cos'è & Perché?**

La lettura degli argomenti da riga di comando è l'azione di estrarre parametri passati durante l'esecuzione di un programma. Fare ciò offre agli sviluppatori la possibilità di personalizzare comportamenti a runtime senza modificare il codice.

**## Come Fare:**

Di seguito, un esempio di come leggere gli argomenti della riga di comando in un'applicazione console C#:

```C#
class Program
{
    static void Main(string[] args)
    {
        foreach (string arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

Se eseguito con il comando `dotnet run arg1 arg2 arg3`, l'output sarà:

```C#
arg1
arg2
arg3
```

**## Approfondimento:**

1. **Contesto storico:** La possibilità di passare argomenti da riga di comando esiste sin dai primi tempi dei sistemi operativi Unix. Divenne un meccanismo standard utilizzato dai programmi per ricevere input quando avviati da un terminale.

2. **Alternative:** Un'alternativa alla lettura diretta degli argomenti da riga di comando è l'uso delle opzioni di configurazione, che possono essere caricate da file di configurazione o variabili d'ambiente.

3. **Dettagli di implementazione:** Quando un'applicazione .NET Core viene avviata da riga di comando, l'array di stringhe `args` all'interno del metodo `Main` viene popolato automaticamente con gli argomenti passati. Gli elementi di `args` corrispondono direttamente agli argomenti passati nel comando.

**## Vedi anche:**

- "Command-Line Arguments" (Argomenti da riga di comando) - Documentazione Microsoft: [link](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/main-and-command-args/#:~:text=in%20C%23-,Main()%20Return%20Values,Application%20object%20in%20WPF%20applications.)
- "How to read command line arguments" (Come leggere gli argomenti della riga di comando) – Tutorial di C# Station: [link](https://csharp-station.com/Tutorial/CSharp)
- "Command Line Arguments In C#" (Argomenti della riga di comando in C#) – C# Corner: [link](https://www.c-sharpcorner.com/UploadFile/a777ce/command-line-arguments-in-C-Sharp/)