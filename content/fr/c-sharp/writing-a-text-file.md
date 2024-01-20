---
title:                "Écriture d'un fichier texte"
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire un fichier texte, c'est sauvegarder des données sous forme de texte lisible. Les développeurs font cela pour des logs, des configurations, ou pour échanger des données simples.

## Comment faire :

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = "exemple.txt";
        string content = "Bonjour, ceci est un texte en exemple.";
        
        File.WriteAllText(path, content);
        Console.WriteLine("Fichier écrit avec succès !");
    }
}
```
Sortie : 
```
Fichier écrit avec succès !
```

## Plongée Profonde

Écrire dans un fichier texte est une pratique depuis les débuts de l'informatique pour stocker information. `System.IO` est un espace de noms en C# contenant des classes pour le traitement de fichiers. Des méthodes alternatives existent, telles que `StreamWriter` ou `File.AppendAllText`, utilisées selon le contexte. L'implémentation dépend de la taille des données et de la fréquence de l'écriture.

## Voir Également

- Documentation Microsoft sur `File.WriteAllText`: [https://learn.microsoft.com/en-us/dotnet/api/system.io.file.writealltext?view=net-6.0](https://learn.microsoft.com/en-us/dotnet/api/system.io.file.writealltext?view=net-6.0)
- Tutoriel sur la manipulation de fichiers en C#: [https://www.tutorialspoint.com/csharp/csharp_file_io.htm](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)