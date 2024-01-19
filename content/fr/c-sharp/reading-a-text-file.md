---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Lire un fichier texte en programmation, ça signifie récupérer et interpréter des informations stockées dans un fichier texte. Les programmeurs le font pour manipuler facilement des données externes, que ce soit pour une utilisation ultérieure dans l'application ou pour communiquer avec d'autres systèmes.

## Comment faire:

En C#, lire un fichier texte est assez simple. Voici un exemple :

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string text = System.IO.File.ReadAllText(@"C:\test.txt");
        System.Console.WriteLine("Contenu du fichier: \n{0}", text);
    }
}
```

Dans cet exemple, votre sortie ressemblera à ceci s'il y a du texte dans votre fichier `C:\test.txt` :

```
Contenu du fichier: 
Bonjour le monde!
```

## Plongée profonde

C#'s `File.ReadAllText` est en réalité une méthode d'une classe plus grande, `System.IO.File`. C'est une surcouche de la méthode `StreamReader.ReadToEnd`, qui existe depuis les débuts de .NET Framework. 

Il existe des alternatives à `File.ReadAllText` comme `File.ReadLines` ou `StreamReader.ReadLine`. Ces méthodes sont utiles quand vous voulez préserver la mémoire ou manipuler des fichiers très volumineux. 

`File.ReadAllText` lit le fichier texte entier en mémoire, ce qui peut s'avérer coûteux en termes de performance pour des fichiers très volumineux. Dans ce cas, l'utilisation de `StreamReader.ReadLine` ou de `File.ReadLines` peut être bénéfique car elles lisent le fichier ligne par ligne plutôt qu'en entier.

## À voir également

- File and Stream I/O: https://docs.microsoft.com/en-us/dotnet/standard/io/
- How to read a text file in C#: https://www.pluralsight.com/guides/how-to-read-a-text-file-in-csharp
- Reading Text File in C#: https://www.c-sharpcorner.com/UploadFile/mahesh/read-write-text-files-C-Sharp/