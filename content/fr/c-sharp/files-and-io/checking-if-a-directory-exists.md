---
title:                "Vérifier si un répertoire existe"
aliases:
- /fr/c-sharp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:07.612132-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Vérifier l'existence d'un répertoire en C# consiste à confirmer la présence d'un dossier à un chemin spécifique dans le système de fichiers. Les programmeurs font cela pour éviter des erreurs telles que tenter de lire ou d'écrire dans un répertoire inexistant, assurant ainsi des manipulations de fichiers et de répertoires plus fluides.

## Comment :

### Utilisation de System.IO

C# fournit l'espace de noms `System.IO` qui contient la classe `Directory`, offrant une manière directe de vérifier l'existence d'un répertoire grâce à la méthode `Exists`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // Vérifier si le répertoire existe
        bool directoryExists = Directory.Exists(directoryPath);

        // Imprimer le résultat
        Console.WriteLine("Le répertoire existe : " + directoryExists);
    }
}
```

**Exemple de sortie :**

```
Le répertoire existe : False
```

Dans le cas où le répertoire existe bien au chemin `C:\ExampleDirectory`, la sortie sera `True`.

### Utilisation de System.IO.Abstractions pour les tests unitaires

Quand il s'agit de rendre votre code testable unitairement, surtout lorsqu'il interagit avec le système de fichiers, le package `System.IO.Abstractions` est un choix populaire. Il vous permet d'abstraire et de simuler les opérations sur le système de fichiers dans vos tests. Voici comment vous pourriez vérifier l'existence d'un répertoire en utilisant cette approche :

D'abord, assurez-vous d'avoir installé le package :

```
Install-Package System.IO.Abstractions
```

Ensuite, vous pouvez injecter un `IFileSystem` dans votre classe et l'utiliser pour vérifier si un répertoire existe, ce qui permet des tests unitaires plus aisés.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Le répertoire existe : " + directoryExists);
    }
}
```

**Exemple de sortie :**

```
Le répertoire existe : False
```

Cette approche découple la logique de votre application de l'accès direct au système de fichiers, rendant votre code plus modulaire, testable et maintenable.
