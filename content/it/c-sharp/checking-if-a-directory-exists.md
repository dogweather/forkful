---
title:                "Verificare l'esistenza di una directory"
html_title:           "C#: Verificare l'esistenza di una directory"
simple_title:         "Verificare l'esistenza di una directory"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Controllare se una directory esiste è una pratica comune nella programmazione che permette di verificare se una determinata directory è presente nel sistema operativo. I programmatori lo fanno per assicurarsi che il loro codice funzioni correttamente e per gestire gli errori che possono verificarsi se una directory non esiste.

## Come fare:
Il seguente codice mostra come controllare se una directory esiste utilizzando C#:
```
if (Directory.Exists(@"C:\Documents"))
{
    Console.WriteLine("La directory esiste!");
}
else
{
    Console.WriteLine("La directory non esiste!");
}
```

Output:
```
La directory esiste! o La directory non esiste!
```

## Approfondimento:
Per molto tempo, la verifica della presenza di una directory è stata una pratica comune nei sistemi operativi. Tuttavia, con lo sviluppo delle tecnologie, sono state introdotte nuove alternative per gestire le directory, come ad esempio l'utilizzo di database o cloud storage. L'implementazione di questo controllo varia anche in base al linguaggio di programmazione utilizzato.

## Vedi anche:
Per maggiori informazioni sul controllo delle directory, puoi consultare i seguenti link:
- [Microsoft Docs - Directory.Exists Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [Tutorialspoint - C# File & Directory Handling](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)