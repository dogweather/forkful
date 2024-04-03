---
date: 2024-01-20 17:55:38.451775-07:00
description: "Les arguments de ligne de commande sont les donn\xE9es que vous balancez\
  \ \xE0 votre programme au d\xE9marrage. Les programmeurs s'en servent pour customiser\u2026"
lastmod: '2024-03-13T22:44:57.804523-06:00'
model: gpt-4-1106-preview
summary: "Les arguments de ligne de commande sont les donn\xE9es que vous balancez\
  \ \xE0 votre programme au d\xE9marrage."
title: Lecture des arguments de ligne de commande
weight: 23
---

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
