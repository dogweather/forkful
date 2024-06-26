---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:06.823694-07:00
description: "Come fare: C# fornisce lo spazio dei nomi `System.IO` che contiene la\
  \ classe `Directory`, offrendo un modo diretto per controllare l'esistenza di una\u2026"
lastmod: '2024-03-13T22:44:43.447996-06:00'
model: gpt-4-0125-preview
summary: C# fornisce lo spazio dei nomi `System.IO` che contiene la classe `Directory`,
  offrendo un modo diretto per controllare l'esistenza di una directory tramite il
  metodo `Exists`.
title: Verifica se una directory esiste
weight: 20
---

## Come fare:


### Utilizzando System.IO
C# fornisce lo spazio dei nomi `System.IO` che contiene la classe `Directory`, offrendo un modo diretto per controllare l'esistenza di una directory tramite il metodo `Exists`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // Controlla se la directory esiste
        bool directoryExists = Directory.Exists(directoryPath);

        // Stampa il risultato
        Console.WriteLine("Directory esiste: " + directoryExists);
    }
}
```

**Output Esempio:**

```
Directory esiste: False
```

Nel caso la directory esista nel percorso `C:\ExampleDirectory`, l'output sarà `True`.

### Utilizzando System.IO.Abstractions per i test unitari
Quando si tratta di rendere il tuo codice testabile unitariamente, specialmente quando interagisce con il file system, il pacchetto `System.IO.Abstractions` è una scelta popolare. Permette di astrarre e simulare le operazioni del file system nei tuoi test. Ecco come potresti controllare l'esistenza di una directory utilizzando questo approccio:

Prima, assicurati di aver installato il pacchetto:

```
Install-Package System.IO.Abstractions
```

Quindi, puoi iniettare un `IFileSystem` nella tua classe e usarlo per controllare se una directory esiste, il che permette test unitari più semplici.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory esiste: " + directoryExists);
    }
}
```

**Output Esempio:**

```
Directory esiste: False
```

Questo approccio disaccoppia la logica della tua applicazione dall'accesso diretto al file system, rendendo il tuo codice più modulare, testabile e mantenibile.
