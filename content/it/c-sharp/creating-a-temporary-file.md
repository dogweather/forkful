---
title:                "C#: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è un'attività comune nella programmazione, utile in molte situazioni come il salvataggio di dati temporanei o la gestione dei file di cache. In questo post, esploreremo come creare e gestire file temporanei nella programmazione in C#.

## Come fare

Creare un file temporaneo in C# è molto semplice utilizzando la classe `System.IO.Path` e il metodo `GetTempFileName()`. Il seguente esempio mostra come creare un file temporaneo:

```C#
using System;
using System.IO;

string tempFile = Path.GetTempFileName();
Console.WriteLine("Il percorso del file temporaneo creato è: " + tempFile);
```

L'output del codice sopra sarà qualcosa del genere:

```
Il percorso del file temporaneo creato è: C:\Users\Username\AppData\Local\Temp\6jxyhk0r.tmp
```

Per scrivere dati all'interno del file temporaneo, possiamo utilizzare la classe `System.IO.File` e il metodo `WriteAllText()` come mostrato nell'esempio seguente:

```C#
using System;
using System.IO;

string tempFile = Path.GetTempFileName();
Console.WriteLine("Il percorso del file temporaneo creato è: " + tempFile);

string text = "Questo è un testo da scrivere nel file temporaneo.";
File.WriteAllText(tempFile, text);
Console.WriteLine("I dati sono stati scritti nel file temporaneo.");
```

Ora se apriamo il file temporaneo, vedremo il testo all'interno di esso.

## Approfondimento

Oltre a creare e scrivere dati in un file temporaneo, è importante anche gestirlo correttamente. Ciò significa eliminare il file dopo averlo utilizzato o assicurarsi che non venga sovrascritto qualora venga utilizzato nuovamente. Per eliminare un file temporaneo, possiamo utilizzare il metodo `Delete()` della classe `System.IO.File`. Ecco un esempio:

```C#
using System;
using System.IO;

string tempFile = Path.GetTempFileName();
Console.WriteLine("Il percorso del file temporaneo creato è: " + tempFile);

string text = "Questo è un testo da scrivere nel file temporaneo.";
File.WriteAllText(tempFile, text);
Console.WriteLine("I dati sono stati scritti nel file temporaneo.");

// Adesso eliminiamo il file temporaneo
File.Delete(tempFile);
Console.WriteLine("Il file temporaneo è stato eliminato.");
```

Se vogliamo, invece, garantire che il file temporaneo non venga sovrascritto in caso di ulteriori utilizzi, possiamo usare il metodo `GetRandomFileName()` della classe `System.IO.Path` per generare un nome di file unico e utilizzarlo come nome per il file temporaneo.

## Vedi anche

- [MSDN Documentation on Path.GetTempFileName()](https://docs.microsoft.com/it-it/dotnet/api/system.io.path.gettempfilename?view=netcore-3.1)
- [MSDN Documentation on File.WriteAllText()](https://docs.microsoft.com/it-it/dotnet/api/system.io.file.writealltext?view=netcore-3.1)
- [MSDN Documentation on File.Delete()](https://docs.microsoft.com/it-it/dotnet/api/system.io.file.delete?view=netcore-3.1)
- [MSDN Documentation on Path.GetRandomFileName()](https://docs.microsoft.com/it-it/dotnet/api/system.io.path.getrandomfilename?view=netcore-3.1)