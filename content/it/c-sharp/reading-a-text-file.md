---
title:                "Lettura di un file di testo"
html_title:           "C#: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere un file di testo è un'operazione fondamentale per i programmatori. Significa aprire un file contenente testo e poterlo leggere, analizzare e manipolare nel codice del nostro programma. Questa operazione è molto utile per lavorare con grandi quantità di dati o per automatizzare la lettura dei dati provenienti da fonti esterne.

## Come fare:
Per leggere un file di testo in C#, possiamo utilizzare la classe ```StreamReader```. Prima di iniziare, dobbiamo assicurarci di aggiungere l'istruzione ```using System.IO;``` all'inizio del nostro codice. Quindi, possiamo utilizzare il seguente codice per leggere un file di testo chiamato "test.txt":

```C#
using (StreamReader sr = new StreamReader("test.txt"))
{
    string line;
    while ((line = sr.ReadLine()) != null)
    {
        Console.WriteLine(line);
    }
}
```

Questo codice aprirà il file "test.txt" e leggerà ogni riga di testo all'interno, stampandole nella console. Una volta terminata l'operazione, il file verrà automaticamente chiuso.

## Approfondimento:
La lettura di file di testo è una funzionalità comune anche in altri linguaggi di programmazione come Java e Python. Inoltre, esistono diverse alternative per leggere i dati da un file di testo in C#, come ad esempio la classe ```File``` o il metodo ```ReadAllLines```. Tuttavia, l'utilizzo di ```StreamReader``` è una delle opzioni più efficienti e flessibili, in quanto consente di leggere il file riga per riga.

Per quanto riguarda l'implementazione, ```StreamReader``` utilizza la classe base ```TextReader``` per leggere il testo dal file, permettendoci di specificare il file di input, il formato di codifica e altre opzioni.

## Vedi anche:
- Documentazione ufficiale di Microsoft su ```StreamReader```: https://docs.microsoft.com/it-it/dotnet/api/system.io.streamreader?view=netcore-3.1
- Tutorial su come leggere file di testo in C#: https://www.c-sharpcorner.com/article/reading-text-file-line-by-line-using-streamreader/
- Domanda su Stack Overflow su come leggere un file di testo in C#: https://stackoverflow.com/questions/11870514/reading-from-a-text-file-with-c-sharp