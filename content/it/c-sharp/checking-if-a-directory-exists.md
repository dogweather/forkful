---
title:                "Verifica se una directory esiste"
html_title:           "C#: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Controllare se una directory esiste in C# significa verificare se un dato percorso nel tuo sistema di file esiste o meno. Questa operazione è spesso necessaria prima di creare nuovi file o directory, di leggere da un file o di apportare modifiche alla directory.

## Come Fare:
Ecco come si può fare in C#. Di seguito è riportato un breve esempio di codice:

```C#
using System.IO;

string path = @"c:\temp\myDir";

if(Directory.Exists(path))
{
    Console.WriteLine("La directory esiste.");
}
else
{
    Console.WriteLine("La directory non esiste.");
}
```

Questo codice controlla se la directory `c:\temp\myDir` esiste. Se esiste, stampa "La directory esiste." Altrimenti, stampa "La directory non esiste."

## Approfondimento
Il metodo `Directory.Exists()` è stato introdotto in .NET Framework 1.0, quindi è disponibile in tutte le versioni di .NET. Offre un modo diretto e semplice per verificare se una directory esiste.

Un'alternativa è l'uso dell'eccezione. Si può tentare di accedere alla directory e catturare un'eccezione `DirectoryNotFoundException` se non esiste. Tuttavia, questa metodologia è generalmente più lenta e richiede più codice.

L'implementazione di `Directory.Exists()` è piuttosto semplice: tenta di aprire la directory specificata e restituisce `true` se ha esito positivo, `false` altrimenti. È sicuro utilizzarlo in un ambiente multithreading poiché non blocca altri thread durante l'esecuzione.

## Vedi Anche
Per ulteriori informazioni sul metodo `Directory.Exists()`, visita la documentazione ufficiale di Microsoft [.NET](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0).

Per ulteriori esempi sull'uso di eccezioni nel controllo dell'esistenza di una directory, consulta [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/how-to-explicitly-throw-exceptions).