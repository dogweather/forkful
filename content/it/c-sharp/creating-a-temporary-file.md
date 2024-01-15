---
title:                "Creazione di un file temporaneo"
html_title:           "C#: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui si potrebbe dover creare un file temporaneo durante la programmazione in C#. Una delle più comuni è per gestire dati temporanei o per effettuare operazioni transitorie durante l'esecuzione di un programma.

## Come fare

Per creare un file temporaneo in C#, è possibile utilizzare la classe `Path` del namespace`System.IO`. Il metodo `GetTempFileName()` restituirà un percorso univoco per un nuovo file temporaneo. Ad esempio:

```C#
string filePath = Path.GetTempFileName();
Console.WriteLine(filePath);
```

Questo codice restituirà una stringa contenente il percorso del nuovo file temporaneo creato. Una volta che il file non è più necessario, è possibile eliminarlo utilizzando il metodo `Delete()` della classe `File`:

```C#
File.Delete(filePath);
```

## Deep Dive

Il metodo `GetTempFileName()` utilizza una combinazione del percorso della directory temporanea e di un numero incrementale per creare un nome univoco per il file temporaneo. Questo significa che ogni volta che viene chiamato, il percorso restituito sarà diverso.

Inoltre, è importante notare che il file creato da `GetTempFileName()` non è completamente vuoto. Infatti, contiene un prefisso di file e una estensione predefiniti, che possono essere utilizzati per identificare il file come temporaneo.

Infine, è importante eliminare il file temporaneo una volta terminato il suo utilizzo, poiché rimarrà nella directory temporanea anche dopo la chiusura del programma.

## Vedi anche

- [Documentazione ufficiale di C# su `Path.GetTempFileName()`](https://docs.microsoft.com/it-it/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
- [Tutorial su come gestire file temporanei in C#](https://www.c-sharpcorner.com/article/how-to-create-temporary-file-in-c-sharp/)