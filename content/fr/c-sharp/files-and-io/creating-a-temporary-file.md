---
date: 2024-01-20 17:40:09.649190-07:00
description: "How to: En C#, cr\xE9er un fichier temporaire est simple. On peut utiliser\
  \ `Path.GetTempFileName()` pour en g\xE9n\xE9rer un nouveau. Voici un petit exemple\
  \ en\u2026"
lastmod: '2024-03-13T22:44:57.808825-06:00'
model: gpt-4-1106-preview
summary: "En C#, cr\xE9er un fichier temporaire est simple."
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

## How to:
En C#, créer un fichier temporaire est simple. On peut utiliser `Path.GetTempFileName()` pour en générer un nouveau. Voici un petit exemple en action :

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string tempFilePath = Path.GetTempFileName();
        Console.WriteLine("Fichier temporaire créé à : " + tempFilePath);

        // Écrivez dans le fichier temporaire
        File.WriteAllText(tempFilePath, "Salut, je suis du texte temporaire!");
        Console.WriteLine("Contenu écrit dans le fichier temporaire.");

        // Lisez et affichez le contenu du fichier temporaire
        string tempFileContent = File.ReadAllText(tempFilePath);
        Console.WriteLine("Contenu du fichier temporaire : " + tempFileContent);

        // Supprimez le fichier temporaire
        File.Delete(tempFilePath);
        Console.WriteLine("Fichier temporaire supprimé.");
    }
}
```

Et voici ce que ça donne à l'exécution :

```
Fichier temporaire créé à : C:\Users\[YourName]\AppData\Local\Temp\tmpA2B3.tmp
Contenu écrit dans le fichier temporaire.
Contenu du fichier temporaire : Salut, je suis du texte temporaire!
Fichier temporaire supprimé.
```

## Deep Dive
Historiquement, créer un fichier temporaire était essentiel pour éviter d'épuiser la mémoire avec des données temporaires. Aujourd'hui, avec plus de mémoire, on pourrait se dire "à quoi bon?", mais c'est toujours utile pour les fichiers volumineux ou pour éviter les conflits dans les environnements multi-utilisateurs.

Alternativement, vous pouvez gérer plus finement vos fichiers temporaires avec `TempFileCollection` ou `FileStream`. Mais attention, une bonne gestion implique la suppression des fichiers une fois qu'ils ne sont plus nécessaires.

Sur le plan de la mise en œuvre, `GetTempFileName()` crée un fichier de nom unique pour éviter les collisions. Les fichiers sont généralement sauvés dans le répertoire temporaire du système, accessible via `Path.GetTempPath()`.

## See Also
Pour aller plus loin, voici des ressources utiles :

- Documentation Microsoft sur la création de fichiers temporaires : [Path.GetTempFileName Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
- Gestion de la mémoire en C# : [Garbage Collection](https://docs.microsoft.com/en-us/dotnet/standard/garbage-collection/)
