---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Lire les arguments de la ligne de commande en C# implique de comprendre et de traiter les données insérées lors de l'exécution d'un programme via le terminal. Les développeurs utilisent cette méthode pour personnaliser le comportement d’un programme en fournissant des paramètres spécifiques au moment de sa lancement.

## Comment faire:

Pour lire les arguments de la ligne de commande dans un programme C#, vous pouvez accéder à l'array `args[]` dans la méthode `Main()`. Voici comment vous pouvez le faire:

```C#
class Program
{
    static void Main(string[] args)
    {
        for (int i = 0; i < args.Length; i++)
        {
            System.Console.WriteLine("Arg[{0}] = [{1}]", i, args[i]);
        }
    }
}
```
Lorsque vous exécutez le programme avec `dotnet run -- arg1 arg2 arg3`, vous obtiendrez ce qui suit:

```
Arg[0] = [arg1]
Arg[1] = [arg2]
Arg[2] = [arg3]
```

## Plongée profonde:

Historiquement, la lecture des arguments de la ligne de commande est une pratique courante en programmation depuis l'ère des systèmes d'exploitation en ligne de commande. 

Une alternative serait d'utiliser une librairie tierce, comme CommandLineParser, qui offre plus de fonctionnalités et facilite le traitement d'options complexes. 

Concernant les détails d'implémentation, à noter que l'array `args` est une copie de la ligne de commande utilisée pour lancer le programme, divisée en segments basés sur les espaces blancs, ce qui peut conduire à des problèmes si vos arguments contiennent eux-mêmes des espaces.

## Voir aussi:

1. La documentation Microsoft sur les arguments de la ligne de commande : https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/main-and-command-args/
2. Un guide sur l'utilisation de la librairie CommandLineParser : https://jalexhaywood.medium.com/a-brief-introduction-to-the-commandlineparser-library-ddc5e1b2e857
3. Un article sur le traitement des options de ligne de commande complexes : https://martinfowler.com/articles/command-line-options.html