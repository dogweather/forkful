---
title:    "C#: Lecture des arguments en ligne de commande"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi 
Les arguments de ligne de commande sont un outil communément utilisé en programmation pour permettre aux utilisateurs d'interagir avec des programmes en leur fournissant des entrées spécifiques. Comprendre comment lire et utiliser ces arguments peut être utile dans de nombreux projets et langages de programmation, y compris C#. Dans cet article, nous allons explorer comment lire des arguments de ligne de commande en utilisant C#.

## Comment faire 
Tout d'abord, il est important de noter que les arguments de ligne de commande sont des informations fournies par l'utilisateur lorsqu'il exécute un programme en utilisant la ligne de commande. Ces arguments peuvent être fournis en utilisant des balises spécifiques comme "-k" ou "/name", suivi de la valeur correspondante. Pour lire ces arguments en C#, nous allons utiliser la méthode "Main" de notre programme, qui est appelée lors de l'exécution du programme. Voici un exemple de code pour lire un argument de ligne de commande en C# :

```C#
static void Main(string[] args)
{
     // lecture d'un argument de ligne de commande
     string argument = args[0];
     Console.WriteLine(argument);
}
```

Dans cet exemple, nous utilisons la variable "args" pour stocker tous les arguments de ligne de commande fournis par l'utilisateur. En utilisant une indexation, nous pouvons accéder à des arguments spécifiques, dans cet exemple, nous récupérons le premier argument et le stockons dans une variable appelée "argument". Ensuite, nous l'affichons simplement à l'écran en utilisant la méthode "WriteLine" de la classe Console.

## Plongée en profondeur 
Maintenant que nous savons comment lire des arguments de ligne de commande en utilisant C#, plongeons un peu plus en détail dans cette fonctionnalité. Une chose à noter est que les arguments de ligne de commande peuvent également être passés sous forme de "chaînes de commande", qui sont simplement une chaîne de caractères contenant tous les arguments séparés par des espaces. Pour les manipuler, nous pouvons utiliser la méthode "Split" pour diviser cette chaîne en une collection d'arguments individuels. Voici un exemple :

```C#
string argumentsChaine = "/name John Doe -k true";
string[] arguments = argumentsChaine.Split(' ');
// arguments = ["/name", "John", "Doe", "-k", "true"]
```

Un autre point important à noter est que les arguments de ligne de commande sont généralement considérés comme des entrées utilisateur non fiables, il est donc important de valider et de nettoyer ces entrées avant de les utiliser dans votre programme.

##Voir aussi
Pour plus d'informations sur la lecture des arguments de ligne de commande en C#, vous pouvez consulter les ressources suivantes :
- [Documentation Microsoft sur la méthode Main en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/main-and-command-args/)
- [Article sur la manipulation des arguments de ligne de commande en C#](https://www.c-sharpcorner.com/uploadfile/mahesh/working-with-command-line-argument-in-C-Sharp/)
- [Vidéo tutoriel sur la lecture d'arguments de ligne de commande en C#](https://www.youtube.com/watch?v=wuTaSD1qwQo)