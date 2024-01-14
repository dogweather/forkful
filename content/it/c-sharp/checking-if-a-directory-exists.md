---
title:    "C#: Verifica dell'esistenza di una directory"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Perché

Controllare se una directory esiste è un'operazione comune quando si lavora con file e cartelle in C#. Se il tuo programma deve fare riferimento a una directory specifica, è importante verificare la sua esistenza prima di tentare di utilizzarla. In questo modo, puoi gestire eventuali errori e assicurarti che il tuo programma funzioni correttamente.

## Come Fare

Per verificare se una directory esiste in C#, puoi utilizzare il metodo `Directory.Exists()` della classe `System.IO`. Questo metodo accetta come parametro una stringa che rappresenta il percorso della directory e restituisce un valore booleano che indica se la directory esiste o meno.

```C#
string directoryPath = @"C:\Users\Utente\Desktop\Progetto";
if(Directory.Exists(directoryPath))
{
    Console.WriteLine("La directory esiste!");
}
else
{
    Console.WriteLine("La directory non esiste.");
}
```

L'`@` prima della stringa ci permette di utilizzare i caratteri di escape nelle directory di Windows, come ad esempio il carattere `\`. Se la directory esiste, verrà stampato il messaggio "La directory esiste!". In caso contrario, verrà stampato il messaggio "La directory non esiste.".

## Approfondimento

Se vuoi controllare se una directory esiste senza dover specificare il percorso completo, puoi utilizzare il metodo `Directory.GetAccessControl()`. Questo metodo restituirà un oggetto `DirectorySecurity` che contiene informazioni sui permessi di accesso alla directory. Se la directory non esiste, verrà lanciata un'eccezione `DirectoryNotFoundException`. Puoi gestire questa eccezione nel tuo codice per assicurarti che il programma non si blocchi se la directory non esiste.

```C#
string directoryName = "Progetto";
try
{
    var accessControl = Directory.GetAccessControl(directoryName);
    Console.WriteLine("La directory esiste!");
}
catch(DirectoryNotFoundException)
{
    Console.WriteLine("La directory non esiste.");
}
```

## Vedi Anche

- Documentazione ufficiale di Microsoft sulla classe `Directory`: https://docs.microsoft.com/it-it/dotnet/api/system.io.directory?view=netcore-3.1
- Tutorial su come gestire le eccezioni in C#: https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/exceptions/