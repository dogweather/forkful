---
title:                "C#: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore C# allora sicuramente avrai bisogno di leggere i file di testo durante lo sviluppo del tuo software. Potresti avere bisogno di estrarre dati dal file o di analizzare il testo per ottenere informazioni importanti. In ogni caso, è essenziale sapere come leggere correttamente un file di testo utilizzando il linguaggio di programmazione C#. 

## Come

La lettura di un file di testo in C# è un'operazione piuttosto semplice. Innanzitutto, è necessario utilizzare la classe File presente nel namespace `System.IO`. Questa classe fornisce metodi per aprire, leggere e scrivere file. Vediamo un esempio di codice:

```C#
string[] lines = File.ReadAllLines("test.txt");

foreach(string line in lines)
{
    Console.WriteLine(line);
}
```

In questo esempio, stiamo leggendo il contenuto del file di testo "test.txt" e lo stiamo stampando sulla console. Nota che il metodo `ReadAllLines()` restituisce un array di stringhe, dove ogni elemento corrisponde ad una riga del file di testo.

## Deep Dive

Oltre al metodo `ReadAllLines()`, la classe `File` fornisce anche altri metodi per la lettura di file di testo. Ad esempio, il metodo `ReadAllText()` restituisce l'intero contenuto del file come una singola stringa, mentre il metodo `ReadLines()` restituisce un `IEnumerable<string>`, che può essere utile se si vuole leggere il file linea per linea senza memorizzarlo interamente in memoria.

Inoltre, è possibile specificare il percorso del file sia come una stringa che come un oggetto `FileInfo`. Ad esempio, invece di "test.txt" potremmo specificare `new FileInfo("test.txt")`.

## Vedi Anche

- [Documentazione Microsoft sulla classe File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [Tutorial su lettura e scrittura di file in C#](https://www.tutorialspoint.com/csharp/csharp_files_io.htm)
- [Esempi di codice su lettura di file in C#](https://www.c-sharpcorner.com/article/read-text-files-in-C-Sharp/)

 Grazie per aver letto questo articolo. Speriamo che ti sia stato d'aiuto nella comprensione di come leggere un file di testo in C#. Buon coding!