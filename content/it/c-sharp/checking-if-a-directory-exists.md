---
title:                "C#: Verifica se una cartella esiste"
simple_title:         "Verifica se una cartella esiste"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Perché dovremmo preoccuparci di verificare se una directory esiste? Semplicemente perché spesso durante la programmazione dobbiamo lavorare con file e directory, quindi è importante essere in grado di controllare se esistono prima di eseguire ulteriori operazioni.

## Come Fare

Per controllare se una directory esiste in C#, possiamo utilizzare il metodo `Directory.Exists` della classe `System.IO`. Ecco un esempio di codice che utilizza questo metodo:

```C#
if (Directory.Exists("C:\\Users\\Public"))
{
    Console.WriteLine("La directory esiste!");
} 
else 
{
    Console.WriteLine("La directory non esiste.");
}
```

In questo esempio, controlliamo se la directory "Public" nella cartella "Users" esiste e stampiamo un messaggio appropriato in base al risultato. 

## Approfondimento

Il metodo `Directory.Exists` controlla solo se la directory esiste, non controlla se è accessibile o se abbiamo i permessi per accedervi. Inoltre, questo metodo restituirà sempre `false` se la directory ha un path di rete. Se hai bisogno di maggiori informazioni sullo stato della directory, puoi utilizzare il metodo `Directory.GetCreationTime` per ottenere la data di creazione della directory o il metodo `Directory.GetAccessControl` per ottenere i permessi di accesso.

## Vedi Anche

- [Documentazione ufficiale di C# per il metodo Directory.Exists](https://docs.microsoft.com/it-it/dotnet/api/system.io.directory.exists)
- [Esempi di codice per lavorare con directory in C#](https://www.c-sharpcorner.com/uploadfile/mahesh/directory-in-c-sharp/)
- [Tutorial su come gestire le eccezioni in C#](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/exceptions/)