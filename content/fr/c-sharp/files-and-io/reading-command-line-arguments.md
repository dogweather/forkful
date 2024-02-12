---
title:                "Lecture des arguments de ligne de commande"
date:                  2024-01-20T17:55:38.451775-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Les arguments de ligne de commande sont les données que vous balancez à votre programme au démarrage. Les programmeurs s'en servent pour customiser l'exécution sans recompilation – pratique pour les configs, les modes de debug, et tout ce qui est input variable.

## How to:
```C#
using System;

class Program {
    static void Main(string[] args) {
        // Vérifie s'il y a des arguments
        if (args.Length > 0) {
            Console.WriteLine("Voici vos arguments:");
            // Boucle pour afficher chaque argument
            foreach (var arg in args) {
                Console.WriteLine(arg);
            }
        } else {
            Console.WriteLine("Aucun argument n'a été fourni.");
        }
    }
}
```
Output avec args (Exemple: `program.exe arg1 arg2`):
```
Voici vos arguments:
arg1
arg2
```
Output sans args:
```
Aucun argument n'a été fourni.
```

## Deep Dive
Historiquement, les arguments de ligne de commande sont les ancêtres des interfaces utilisateur graphiques. Les alternatives incluent la configuration via des fichiers, des variables d'environnement ou des entrées utilisateur en cours d’exécution.

En C#, les arguments sont accessibles via le tableau `string[] args` dans la méthode `Main`. On pourrait les lire avec `Environment.GetCommandLineArgs()`, mais cette méthode inclut aussi le nom de l'exécutable, contrairement à `args`.

L'indexation des `args` commence par zéro. Si vous attendez un type spécifique d'argument (comme un entier), vous devez le convertir avec `int.Parse(args[i])`, en faisant attention aux exceptions possibles.

## See Also
- Microsoft docs sur les arguments de ligne de commande en C#: [docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/main-and-command-args/](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/main-and-command-args/)
- Plus sur `Environment.GetCommandLineArgs()`: [docs.microsoft.com/fr-fr/dotnet/api/system.environment.getcommandlineargs](https://docs.microsoft.com/fr-fr/dotnet/api/system.environment.getcommandlineargs)
- Guide sur les parsing libs pour gérer les arguments: [CommandLineParser GitHub](https://github.com/commandlineparser/commandline)
