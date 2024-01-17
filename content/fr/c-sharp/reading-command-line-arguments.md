---
title:                "Lecture des arguments de ligne de commande"
html_title:           "C#: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est? 

Lecture des arguments de ligne de commande est essentiellement le processus de récupération des valeurs saisies par l'utilisateur lorsqu'il exécute un programme à partir de la ligne de commande. Les programmeurs font cela pour pouvoir personnaliser l'exécution de leur programme en fonction des entrées de l'utilisateur.

## Comment faire :

Voici un exemple simple de lecture des arguments de ligne de commande en C# :

```C#
using System;

public class Program
{
    public static void Main(string[] args)
    {
        // On utilise la classe Environment pour accéder aux arguments de ligne de commande
        string[] arguments = Environment.GetCommandLineArgs();

        Console.WriteLine("Argument 1 : {0}", arguments[0]); // affiche le nom du programme
        Console.WriteLine("Argument 2 : {0}", arguments[1]); // affiche le premier argument saisi par l'utilisateur
    }
}
```
Exemple d'entrée utilisateur : ```monProgramme.exe argument1 argument2```

Résultat :
```
Argument 1 : monProgramme.exe
Argument 2 : argument1
```

## Plongée en profondeur :

La lecture des arguments de ligne de commande peut sembler un concept simple, mais il est très utile dans de nombreux scénarios de programmation. Il peut être utilisé pour personnaliser l'exécution d'un programme en fonction des préférences de l'utilisateur, pour effectuer des opérations spécifiques en fonction des arguments saisis ou encore pour faciliter le processus de débogage.

Dans le passé, certains programmeurs utilisaient la méthode Console.Read() pour récupérer les entrées de l'utilisateur, mais cette méthode n'était pas très pratique et pouvait être source d'erreurs. La lecture des arguments de ligne de commande est donc devenue la méthode préférée pour récupérer les entrées de l'utilisateur.

## Voir aussi :

Vous pouvez en apprendre davantage sur la lecture des arguments de ligne de commande en consultant la documentation officielle de Microsoft sur ce sujet : https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments

Vous pouvez également trouver des exemples pratiques et des tutoriels sur des sites tels que tutorialspoint.com ou c-sharpcorner.com. N'hésitez pas à expérimenter et à découvrir toutes les possibilités offertes par la lecture des arguments de ligne de commande en C#.