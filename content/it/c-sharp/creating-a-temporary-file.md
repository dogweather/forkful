---
title:    "C#: Creazione di un file temporaneo"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può essere utile quando si desidera creare un file temporaneo durante l'esecuzione di un programma per archiviare temporaneamente dei dati prima di salvarli definitivamente. Può anche essere utile quando si desidera scrivere su un file temporaneo invece che utilizzare una stringa, ad esempio quando si lavora con grandi quantità di dati.

## Come fare

Per creare un file temporaneo in C#, è possibile utilizzare il metodo `Path.GetTempFileName()` che crea un file temporaneo vuoto nella posizione predefinita del sistema per i file temporanei. Il nome del file è generato automaticamente e sarà unico. Ecco un esempio di come utilizzare questo metodo:

```C#
string fileName = Path.GetTempFileName();

Console.WriteLine(fileName); // Output: C:\Users\UserName\AppData\Local\Temp\tmp12E3.tmp
```

Il metodo `Path.GetTempFileName()` restituisce il percorso completo del file temporaneo creato.

## Approfondimento

Esistono diverse opzioni per personalizzare il file temporaneo creato utilizzando il metodo `Path.GetTempFileName()`. Ad esempio, è possibile specificare una directory diversa per la creazione del file temporaneo utilizzando il metodo `Path.GetTempPath()` che restituisce il percorso predefinito del sistema per i file temporanei. Inoltre, è possibile specificare un prefisso per il nome del file temporaneo utilizzando il metodo `Path.GetRandomFileName()`.

```C#
string tempDir = Path.GetTempPath();
string prefix = "myTempFile_";
string fileName = Path.Combine(tempDir, prefix + Path.GetRandomFileName());

Console.WriteLine(fileName); // Output: C:\Users\UserName\AppData\Local\Temp\myTempFile_gztqm4ft.akz
```

Inoltre, è possibile specificare l'estensione del file utilizzando il metodo `Path.ChangeExtension()`.

```C#
string tempDir = Path.GetTempPath();
string extension = ".txt";
string fileName = Path.ChangeExtension(Path.GetRandomFileName(), extension);
fileName = Path.Combine(tempDir, fileName);

Console.WriteLine(fileName); // Output: C:\Users\UserName\AppData\Local\Temp\wrhlldjy.txt
```

## Vedi anche

- [Documentazione Microsoft su Path Class](https://docs.microsoft.com/it-it/dotnet/api/system.io.path?view=net-5.0) 
- [Guida introduttiva su file e directory in C#](https://www.c-sharpcorner.com/uploadfile/mahesh/the-path-class-in-C-Sharp/)