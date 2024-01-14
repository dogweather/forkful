---
title:    "C#: Lecture des arguments de ligne de commande"
keywords: ["C#"]
---

{{< edit_this_page >}}

##Pourquoi

Si vous avez déjà utilisé un programme en ligne de commande, vous avez peut-être remarqué que certains programmes prennent des arguments en entrée. Ces arguments peuvent être utilisés pour personnaliser le programme ou lui donner des instructions spécifiques. Mais comment les lire dans votre code ? Dans cet article, nous allons vous montrer comment lire les arguments de ligne de commande en utilisant le langage de programmation C#.

##Comment faire

La première étape pour lire les arguments de ligne de commande en C# est de déclarer un tableau de chaînes de caractères pour stocker les arguments. Ce tableau sera passé en tant que paramètre à la méthode Main() de votre programme. Pour lire les arguments, vous pouvez utiliser la classe statique Environment, qui fournit une méthode GetCommandLineArgs() pour récupérer tous les arguments en tant que tableau de chaînes de caractères.

Voici un exemple de code pour lire les arguments et les afficher à l'écran :

```C#
static void Main(string[] args)
{
   string[] arguments = Environment.GetCommandLineArgs();

   // parcourir les arguments et les afficher
   for (int i = 0; i < arguments.Length; i++)
   {
       Console.WriteLine("Argument {0}: {1}", i + 1, arguments[i]);
   }
}
```

Si vous exécutez ce programme avec plusieurs arguments de ligne de commande, vous verrez leur valeur affichée à l'écran.

```
$ dotnet run arg1 arg2
Argument 1: arg1
Argument 2: arg2
```

Vous pouvez également utiliser des arguments optionnels en utilisant le préfixe "-" ou "/" suivi du nom de l'argument et de sa valeur. Pour lire les arguments optionnels, vous pouvez utiliser une boucle for avec un pas de 2 pour récupérer chaque nom et valeur d'argument séparément.

Par exemple :

```C#
static void Main(string[] args)
{
   for (int i = 0; i < args.Length; i += 2)
   {
       string argName = args[i];
       string argValue = args[i + 1];
       Console.WriteLine("Argument {0}: {1}", argName, argValue);
   }
}
```

##Plongée profonde

Au-delà de la lecture des arguments de ligne de commande, il est également possible de spécifier des options avec des valeurs par défaut, ainsi que des arguments de ligne de commande pour les options. Pour cela, vous pouvez utiliser la classe OptionSet de la bibliothèque Mono.Options. Cette bibliothèque vous permet de spécifier des options et des arguments avec une syntaxe propre et d'obtenir les valeurs correspondantes dans votre code.

Voici un exemple de code utilisant Mono.Options pour lire des options et des arguments de ligne de commande :

```C#
// déclaration des options et des arguments
ArgParser parser = new ArgParser();
string option1 = parser.AddOption("option1", "default1", "Description de l'option 1");
string option2 = parser.AddOption("option2", "default2", "Description de l'option 2");

// récupérer la valeur d'une option
string valOption1 = parser.Get(option1);

// récupérer la valeur d'un argument
string arg1 = parser.GetArgument(0);
```

##Voir aussi

- [Documentation officielle de C#](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [Documentation de Mono.Options](https://github.com/xamarin/XamarinComponents/tree/master/XPlat/Mono.Options)