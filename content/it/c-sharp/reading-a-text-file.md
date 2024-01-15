---
title:                "Leggere un file di testo"
html_title:           "C#: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
Ci sono diversi motivi per cui potresti voler leggere un file di testo utilizzando il linguaggio di programmazione C#. Potresti avere bisogno di estrarre dati da un file di log o di creare un programma per la lettura di file di configurazione.

## Come Leggere un File di Testo con C#
Per leggere un file di testo utilizzando C#, puoi utilizzare il metodo `File.ReadAllText()` che accetta come parametro il percorso del file da leggere. Ad esempio:

```C#
string content = File.ReadAllText(@"C:\Users\NomeUtente\Documenti\file.txt");
```

Il contenuto del file verrà letto e salvato nella variabile `content` come una stringa.

Puoi anche utilizzare il metodo `File.ReadAllLines()` per leggere ogni riga del file e salvarle in un array di stringhe, utilizzando il ciclo `foreach` per iterare su ogni riga. Ad esempio:

```C#
string[] lines = File.ReadAllLines(@"C:\Users\NomeUtente\Documenti\file.txt");

foreach (string line in lines)
{
    Console.WriteLine(line);
}
```

In questo modo, ogni riga del file verrà stampata sulla console.

## Approfondimento sulla Lettura di File di Testo
Oltre ai metodi `ReadAllText()` e `ReadAllLines()`, esistono altri modi per leggere un file di testo utilizzando C#. Ad esempio, è possibile utilizzare la classe `StreamReader` per leggere il file riga per riga, o utilizzare `using` per garantire che il file venga chiuso automaticamente dopo la lettura.

Inoltre, è possibile specificare il tipo di codifica del file di testo utilizzando il parametro opzionale `Encoding`, in modo da gestire correttamente caratteri speciali o diverse lingue.

## Vedi Anche
- [File.ReadAllText() documentazione su Microsoft Docs](https://docs.microsoft.com/it-it/dotnet/api/system.io.file.readalltext)
- [File.ReadAllLines() documentazione su Microsoft Docs](https://docs.microsoft.com/it-it/dotnet/api/system.io.file.readalllines)
- [StreamReader documentazione su Microsoft Docs](https://docs.microsoft.com/it-it/dotnet/api/system.io.streamreader)