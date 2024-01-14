---
title:                "C#: Verifica dell'esistenza di una directory"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler verificare se una directory esiste. Ad esempio, potresti avere bisogno di creare una nuova directory per archiviare dei file o di verificare se una directory predefinita è stata correttamente configurata prima di avviare il tuo programma. Indipendentemente dal motivo, sapere come controllare l'esistenza di una directory è un'abilità fondamentale per ogni programmatore.

## Come fare
Per verificare se una directory esiste in C#, puoi utilizzare il metodo statico di classe `Directory.Exists()` della libreria `System.IO`. Sono necessari due semplici passaggi: prima di tutto, bisogna specificare il percorso completo della directory che si vuole controllare; poi bisogna confrontare il valore restituito dal metodo con il valore booleano `true`. Ecco un esempio di codice che ti mostrerà come farlo:

```C#
string directory = @"C:\Users\Utente\Desktop\MiaCartella";
if (Directory.Exists(directory))
{
    Console.WriteLine($"La directory {directory} esiste.");
}
else
{
    Console.WriteLine($"La directory {directory} non esiste.");
}
```

Se la directory esiste, l'output sarà: `La directory C:\Users\Utente\Desktop\MiaCartella esiste.` Altrimenti, se la directory non esiste, verrà stampato `La directory C:\Users\Utente\Desktop\MiaCartella non esiste.`

## Approfondimento
Perché utilizzare il metodo `Directory.Exists()` invece di impostare un try-catch e gestire un'eccezione `DirectoryNotFoundException`? Semplice, perché il primo è molto più efficiente in termini di prestazioni. Gestire un'eccezione richiede risorse aggiuntive per il sistema ed è considerevolmente più lento rispetto al semplice controllo di un valore booleano. Inoltre, utilizzare il metodo `Directory.Exists()` è più leggibile e facile da comprendere per altri programmatori che potrebbero leggere il tuo codice.

## Vedi anche
- [Documentazione ufficiale di Microsoft su `Directory.Exists()`](https://docs.microsoft.com/it-it/dotnet/api/system.io.directory.exists)
- [Come creare una nuova directory in C#](https://www.lanouvellecentrafrique.org/come-creare-una-nuova-directory-in-c/)
- [Come eliminare una directory in C#](https://docs.microsoft.com/it-it/dotnet/api/system.io.directory.delete)