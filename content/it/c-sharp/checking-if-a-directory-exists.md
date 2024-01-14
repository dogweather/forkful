---
title:    "C#: Verifica dell'esistenza di una directory"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si scrive un programma in C#, può essere necessario controllare se una directory esiste o meno prima di eseguire una determinata operazione su di essa. Questo può essere fatto per evitare errori durante l'esecuzione del programma o per garantire che il programma funzioni correttamente.

## Come Fare

Per verificare se una directory esiste in C#, è possibile utilizzare il metodo "Directory.Exists" della classe "System.IO". Questo metodo accetta come parametro il percorso della directory e restituisce un valore booleano "true" se la directory esiste, altrimenti restituisce "false".

Esempio di codice:

```C#
string path = @"C:\Users\Esempio\Documenti\";
if (Directory.Exists(path)) 
{
    Console.WriteLine("La directory esiste!");
}
else
{
    Console.WriteLine("La directory non esiste.");
}
```

Output:

```
La directory esiste!
```

## Approfondimento

È importante notare che il metodo "Directory.Exists" effettua solo una verifica sulla presenza della directory, ma non garantisce che la directory possa essere acceduta o scritta. Inoltre, se il percorso specificato non è corretto, il metodo restituirà comunque "false".

Inoltre, questo metodo può essere utilizzato anche per controllare la presenza di file, poiché i file possono essere considerati come directory nel sistema operativo. Ciò significa che è possibile utilizzare lo stesso codice per controllare sia le directory che i file.

## Vedi Anche

- Documentazione ufficiale sulla classe "System.IO.Directory"
- Tutorial su come gestire le directory e i file in C#