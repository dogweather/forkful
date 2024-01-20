---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creazione di un file temporaneo in C#

## Che cos'è e perché?

La creazione di un file temporaneo è un'operazione che permette di generare un file destinato ad un uso temporaneo. Gli sviluppatori ne fanno uso principalmente per conservare dati temporanei, quando i dati sono troppo grandi per essere mantenuti in memoria.

## Come fare:

La creazione di un file temporaneo in C# è abbastanza semplice grazie al metodo `Path.GetTempFileName()`. Ecco un esempio di come utilizzarlo:

```C#
using System.IO;

class Program
{
    static void Main()
    {
        string tempFile = Path.GetTempFileName();
        Console.WriteLine("File temporaneo creato in: " + tempFile);
    }
}
```

Dopo aver eseguito il codice, vedrai stampata una riga simile a:
```
File temporaneo creato in: C:\Users\Username\AppData\Local\Temp\tmpAB12.tmp
```

## Approfondimento

### Un po' di storia

La creazione di file temporanei ha origine dai primi tempi del computing, quando la memoria era un risorsa molto limitata. Per circumnavigare questi limiti, i dati temporanei erano frequentemente scritti su un disco.

### Alternative

Anche se `Path.GetTempFileName()` è un metodo conveniente, ci sono altre opzioni. La classe `FileOptions` del namespace `System.IO` contiene l'opzione `DeleteOnClose`, che consente il cancellarsi automaticamente del file quando tutte le referenze al file sono rilasciate.

```C#
using (FileStream fs = new FileStream(Path.GetTempFileName(), 
                                      FileMode.Create, 
                                      FileAccess.Write, 
                                      FileShare.None, 
                                      4096, 
                                      FileOptions.DeleteOnClose))
{
    // usa il file...
}
```

### Dettagli Implementazione 

Quando chiami `Path.GetTempFileName()`, genera un file con estensione `.tmp` e un nome univoco, e lo crea nella directory temporanea dell'utente.

## Altre Risorse

Per ulteriori informazioni sulla creazione di file temporanei e sulla gestione dei file in C#, consulta le risorse seguenti:

- [Documentazione ufficiale Microsoft](https://docs.microsoft.com/it-it/dotnet/api/system.io.path.gettempfilename)
- [Flussi di file e I/O](https://msdn.microsoft.com/it-it/library/k3352a4w(vs.71).aspx)