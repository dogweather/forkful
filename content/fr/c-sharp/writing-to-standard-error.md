---
title:    "C#: Écrire sur l'erreur standard"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi 

Ecrire sur la sortie d'erreur standard est une pratique courante dans le développement logiciel en C#. Cela permet aux programmeurs de mieux comprendre les erreurs et les bugs dans leur code, ce qui les aide à les résoudre plus rapidement et efficacement. 

## Comment faire 

Voici un exemple de code en C# montrant comment écrire sur la sortie d'erreur standard : 

```C#
Console.Error.WriteLine("Une erreur s'est produite : Erreur X");
``` 

Ceci affichera le message "Une erreur s'est produite : Erreur X" sur la sortie d'erreur standard. Vous pouvez également utiliser la méthode `Console.Error.WriteLineFormat()` pour formater votre message avec des variables : 

```C#
int erreurCode = 404;
Console.Error.WriteLineFormat("Une erreur s'est produite : Erreur {0}", erreurCode);
```

Cela affichera "Une erreur s'est produite : Erreur 404" sur la sortie d'erreur standard. 

## Plongée en profondeur 

Il est important de noter que la sortie d'erreur standard affiche uniquement les messages d'erreur et non les captures d'exception. Pour cela, vous devrez utiliser `Console.Error.WriteLine()` dans un bloc `try-catch` pour attraper l'exception. Vous pouvez également rediriger la sortie d'erreur standard vers un autre flux à l'aide de la propriété `Console.SetError()`. De plus, la sortie d'erreur standard peut être utilisée pour afficher des messages d'avertissement ou d'informations en utilisant `Console.Error.WriteLine()`. 

## Voir aussi 

- [Documentation Microsoft sur la sortie d'erreur standard en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.console.error)
- [Tutoriel sur la gestion des exceptions en C#](https://www.c-sharpcorner.com/article/exception-handling-in-C-Sharp/)