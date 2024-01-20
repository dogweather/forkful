---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Créer un fichier temporaire est une pratique consistant à créer un fichier à usage unique pour stocker provisoirement des données. Les programmeurs font cela pour manipuler les données sans altérer le flux d'information principal.

## Comment faire:

Voici une façon de créer un fichier temporaire en C# :

```C#
using System.IO;

public class TempFileDemo
{
    public void CreateTempFile()
    {
        string tempFile = Path.GetTempFileName();

        using (StreamWriter sw = new StreamWriter(tempFile))
        {
            sw.WriteLine("Voici un fichier temporaire en C#!");
        }

        using (StreamReader sr = new StreamReader(tempFile))
        {
            string line = sr.ReadLine(); 
            Console.WriteLine(line);
        }
    }
}
```
Lorsque vous exécutez ce code, il crée un fichier temporaire et écrit la ligne "Voici un fichier temporaire en C#!" puis la lit et l'affiche à la console.

## Deep Dive:

Historiquement, l'utilisation de fichiers temporaires provient d'une époque où la mémoire était une ressource limitée. Aujourd'hui, ils sont souvent utilisés pour stocker des données temporaires de grande taille ou pour manipuler des données dans le cadre d'une opération de type transactionnel.

Un chemin alternatif serait l'utilisation de la mémoire (par exemple, un tableau ou une liste) pour stocker temporairement les données. Cependant, cette option a ses limites, comme l'incapacité à gérer de grandes quantités de données.

La méthode `Path.GetTempFileName()` en C# crée un fichier temporaire avec un nom unique dans le répertoire temporaire de votre système et renvoie le chemin d'accès complet à ce fichier. Le fichier est créé vide, mais avec une taille réservée de 0 octet.

## Voir aussi:

- [Documentation sur `Path.GetTempFileName()` (anglais)](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)