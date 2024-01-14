---
title:                "C#: Lecture des arguments de ligne de commande"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur C# ou si vous êtes en train d'apprendre ce langage, vous pourriez vous demander pourquoi vous devriez accorder de l'importance à la lecture des arguments de ligne de commande. Après tout, il existe de nombreux moyens de fournir des données à un programme, pourquoi est-il important de savoir comment lire les arguments de la ligne de commande ? La réponse est simple : c'est un moyen efficace et pratique de fournir des données à votre programme lors de son exécution. Au lieu de modifier constamment votre code pour changer les valeurs des variables, vous pouvez simplement les passer en tant qu'arguments de la ligne de commande. Cela permet également à votre programme d'être utilisé de manière plus dynamique et polyvalente.

## Comment faire

Avant d'apprendre à lire les arguments de la ligne de commande, il est important de comprendre comment ils sont formatés. Les arguments de la ligne de commande sont généralement passés sous la forme d'une chaîne de caractères, avec chaque argument étant séparé par un espace. Par exemple : `monprogramme.exe argument1 argument2`.

Maintenant, passons à la partie pratique. Pour lire les arguments de la ligne de commande, tout d'abord, nous devons importer l'espace de noms `System`, qui contient la classe `Environment`. Cette classe contient la méthode `GetCommandLineArgs` qui nous permet de récupérer les arguments de la ligne de commande sous forme d'un tableau de chaînes de caractères. Voici un exemple de code pour la lecture et l'affichage des arguments de la ligne de commande :

```C#
using System;

namespace LectureArgumentsLigneDeCommande
{
    class Program
    {
        static void Main(string[] args)
        {
            // Récupère les arguments de la ligne de commande
            string[] arguments = Environment.GetCommandLineArgs();

            // Boucle à travers les arguments et les affiche
            for (int i = 0; i < arguments.Length; i++)
            {
                Console.WriteLine($"Argument {i}: {arguments[i]}");
            }
        }
    }
}
```

Si nous exécutons ce programme avec les arguments `argument1 argument2`, nous obtiendrons la sortie suivante :

```
Argument 0: Chemin du programme.exe
Argument 1: argument1
Argument 2: argument2
```

Comme vous pouvez le voir, le premier argument `argument0` est toujours le chemin d'accès vers le programme lui-même, tandis que les arguments suivants sont passés par l'utilisateur.

## Plongée profonde

Il est également important de noter que nous pouvons récupérer et utiliser les arguments de manière plus spécifique en utilisant la méthode `GetCommandLineArgs` de la classe `Environment`. Cette méthode renvoie un tableau de chaînes de caractères, mais nous pouvons utiliser la méthode `GetCommandLineArg` pour récupérer un argument spécifique en utilisant son index.

En outre, si vous avez des arguments avec des espaces (par exemple, `monprogramme.exe "argument avec espace"`), vous pouvez utiliser la méthode `GetCommandLineArgs` pour récupérer l'argument entier, y compris les espaces.

## Voir aussi

- [Documentation officielle sur la classe `Environment.GetCommandLineArgs`](https://docs.microsoft.com/fr-fr/dotnet/api/system.environment.getcommandlineargs?view=net-5.0)
- [Exemples de code pour lire les arguments de la ligne de commande en C#](https://www.c-sharpcorner.com/article/parsing-command-line-arguments-in-c-sharp/)