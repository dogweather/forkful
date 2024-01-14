---
title:                "C#: Leggere un file di testo"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

La lettura di file di testo è una delle attività più comuni nel mondo della programmazione. È fondamentale per lavorare con una vasta gamma di dati e per automatizzare processi come l'importazione e l'esportazione di informazioni. In questo articolo scopriremo come leggere un file di testo utilizzando il linguaggio di programmazione C#.

## Come fare

Per leggere un file di testo in C#, il primo passo è aprire il file utilizzando la classe `StreamReader`. Questa classe fornisce una varietà di metodi per leggere dati da un file di testo, tra cui il metodo `ReadLine()` che legge una riga alla volta.

```C#
using (StreamReader sr = new StreamReader(@"C:\esempio.txt"))
{
    string line = sr.ReadLine();
    Console.WriteLine(line);
}
```

Nel codice sopra, stiamo utilizzando il costrutto `using` per garantire che il `StreamReader` venga chiuso correttamente dopo aver letto il file. Il metodo `ReadLine()` ci permette di ottenere una stringa contenente il contenuto della prima riga del file.

Questo è solo un esempio di base, ma ci sono molti altri metodi e opzioni disponibili per la lettura di file di testo in C#. È importante tenere presente che il codice sopra presume che il file si trovi nella cartella specificata e che il file sia leggibile dal programma.

## Approfondimento

Per coloro che desiderano approfondire conoscenze sulle operazioni di lettura di file di testo in C#, ci sono molti aspetti in cui possiamo esplorare ulteriormente. Ad esempio, possiamo utilizzare i metodi `File.ReadAllBytes()` e `File.WriteAllText()` per leggere e scrivere in modo efficiente tutto il contenuto di un file di testo. Oppure, possiamo utilizzare il costrutto `using` con la classe `FileStream` per leggere caratteri binari e altri dati dal file.

Un'altra cosa importante da considerare è la gestione degli errori durante la lettura del file di testo. In caso di errori, come ad esempio un file inesistente o un file danneggiato, è necessario implementare una gestione degli errori robusta nel codice in modo che il programma non si blocchi.

## Vedi anche

- [Documentazione di Microsoft su StreamReader](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)
- [Esempi di lettura di file di testo in C#](https://www.c-sharpcorner.com/article/c-sharp-basics-how-to-read-text-files/)
- [Tutorial su File Handling in C#](https://www.tutorialsteacher.com/csharp/csharp-file-io)