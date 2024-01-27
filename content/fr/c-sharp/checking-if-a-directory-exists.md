---
title:                "Vérification de l'existence d'un répertoire"
date:                  2024-01-19
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Vérifier l'existence d'un répertoire, c'est s'assurer qu'un dossier est bien là où on pense qu'il est. On fait ça pour éviter des erreurs, comme essayer de lire ou d'écrire dans un répertoire qui n'existe pas.

## How to (Comment faire ?)
En C#, on utilise la classe `System.IO.Directory` et sa méthode `Exists` pour vérifier l'existence d'un répertoire :

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\UnDossier";
        
        if (Directory.Exists(path))
        {
            Console.WriteLine("Le répertoire existe !");
        }
        else
        {
            Console.WriteLine("Le répertoire n'existe pas.");
        }
    }
}
```
Sortie possible :
```
Le répertoire existe !
```
ou
```
Le répertoire n'existe pas.
```

## Deep Dive (Plongée en profondeur)
L'usage de la méthode `Exists` remonte aux débuts de .NET. Elle constitue un moyen sûr et fiable d'éviter des erreurs de fichier courantes. Des alternatives incluent la création de try-catch autour des opérations de fichier, mais c'est lourd. Utiliser `Exists` est direct et efficace. En interne, `Exists` fait appel aux API systèmes pour vérifier l'existence physique des fichiers, cela peut impliquer une certaine latence due aux appels système et il faut être conscient des permissions nécessaires pour accéder aux répertoires.

## See Also (Voir aussi)
- Documentation Microsoft sur `Directory.Exists` : [msdn.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- Classe `DirectoryInfo` pour plus d'informations sur un répertoire : [msdn.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo)
- Gestion des erreurs et des exceptions en C# : [msdn.microsoft.com](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
