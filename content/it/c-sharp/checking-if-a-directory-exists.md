---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-19
html_title:           "Arduino: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Controllare l'esistenza di una directory significa verificare che un determinato percorso sul file system sia effettivamente una cartella esistente. I programmatori lo fanno per evitare errori e intervenire correttamente: per esempio, prima di scrivere un file o prima di elencare il contenuto di una directory.

## How to:
In C#, la classe `System.IO.Directory` fornisce il metodo `Exists`, che verifica se la directory specificata esiste. Ecco un esempio semplice:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\un-percorso\una-directory";
        
        if (Directory.Exists(path))
        {
            Console.WriteLine("La directory esiste.");
        }
        else
        {
            Console.WriteLine("La directory non esiste.");
        } 
    }
}
```

Output potrebbe essere:
```
La directory esiste.
```
o
```
La directory non esiste.
```
in base al fatto che il percorso specificato sia presente sul sistema o meno.

## Deep Dive:
Storicamente, la verifica dell'esistenza di una directory ha sempre giocato un ruolo cruciale per evitare errori durante operazioni sui file. Alternativamente, ci sono delle eccezioni, come `DirectoryNotFoundException`, che si possono intercettare per gestire i casi in cui una directory non esiste. Tuttavia, controllare prima di agire viene generalmente considerato una buona pratica—non solo previene errori, ma rende il codice più leggibile e meno propenso ad eccezioni inaspettate.

A livello di implementazione, `Directory.Exists` ritorna `false` sia se la directory non esiste, sia se il chiamante non ha i permessi per accedere a quel percorso. È importante ricordarsi di gestire adeguatamente i permessi per evitare confusioni.

## See Also:
Per approfondire la classe `System.IO.Directory` e i suoi metodi, si possono consultare le seguenti risorse:

- Documentazione ufficiale di `Directory.Exists`: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- Gestione delle eccezioni in IO: [Microsoft Docs - IOException](https://docs.microsoft.com/en-us/dotnet/api/system.io.ioexception)
- Linee guida sulle buone pratiche di IO in .NET: [Microsoft Docs - File and Stream I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/)
