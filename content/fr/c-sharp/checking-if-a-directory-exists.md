---
title:                "Vérifier si un répertoire existe"
html_title:           "C#: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi? 

Vérifier si un répertoire existe est une opération de base en programmation C#. Les programmeurs le font pour éviter des erreurs d'exécution qui peuvent survenir lorsqu'ils essaient d'accéder, de lire ou d'écrire dans un répertoire qui n'existe pas.

## Comment faire:

Pour vérifier si un répertoire existe en C#, on utilise principalement la méthode `Directory.Exists()` de la classe `System.IO`. Voici un exemple de code qui montre comment cela fonctionne:

```C#
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\example\directory";
        
        if (Directory.Exists(path))
        {
            System.Console.WriteLine("Le répertoire existe.");
        }
        else
        {
            System.Console.WriteLine("Le répertoire n'existe pas.");
        }
    }
}
```

Si le répertoire "C:\example\directory" existe, le programme affichera "Le répertoire existe." Sinon, il dira "Le répertoire n'existe pas."

## Plongée profonde

La méthode `Directory.Exists()` existe depuis le début du .NET Framework. Sa principale alternative est de tenter une opération comme 'Open', 'Read' ou 'Write', et de traiter les exceptions qui se produisent si le répertoire n'existe pas. Cependant, cette approche est généralement moins efficace que de simplement utiliser `Directory.Exists()`.

L'implémentation de la méthode `Directory.Exists()` est relativement simple. Elle utilise des appels API Windows pour essayer d'ouvrir le répertoire spécifié. Si l'appel réussit, le répertoire existe; sinon, il n'existe pas.

## Voir aussi

Pour des informations supplémentaires sur la vérification de l'existence d'un répertoire en C#, consultez ces ressources :

- [Documentation Microsoft sur Directory.Exists](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Guide MSDN pour la gestion des exceptions d'opérations de fichiers et de répertoires](https://docs.microsoft.com/fr-fr/dotnet/standard/io/handling-io-errors)

Garçon, rappelez-vous, la vérification de l'existence d'un répertoire est une étape cruciale pour éviter les bugs inattendus et les crashs de vos applications !