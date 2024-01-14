---
title:                "C#: Écrire vers l'erreur standard"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi écrire vers la sortie standard d'erreur en C# ?

Le processus de développement d'une application ou d'un logiciel peut être complexe et délicat. Cela implique souvent de trouver des bugs et des erreurs de code qui peuvent être difficiles à détecter. L'une des techniques les plus utiles pour le débogage est l'écriture vers la sortie standard d'erreur en utilisant le langage de programmation C#. Cela peut sembler un peu intimidant au début, mais une fois que vous aurez compris comment le faire, cela peut vous faire gagner un temps précieux lors du débogage de votre code.

## Comment écrire vers la sortie standard d'erreur en C# ?

Pour écrire vers la sortie standard d'erreur en C#, vous pouvez utiliser la méthode Console.Error.WriteLine(). Voici un exemple de code pour afficher une erreur :

```C#
Console.Error.WriteLine("Une erreur est survenue : {0}", errorMessage);
```

Vous pouvez également utiliser la méthode Console.Error.Write() si vous souhaitez simplement écrire le message d'erreur sans sauter une ligne :

```C#
Console.Error.Write("Une erreur est survenue : ");
Console.Error.Write(errorMessage);
```

Lorsque vous exécutez ce code, le message d'erreur sera affiché dans la sortie standard d'erreur au lieu de la sortie standard normale. Ainsi, il sera plus facile à repérer et à suivre lors du débogage de votre application.

## Plongée approfondie dans l'écriture vers la sortie standard d'erreur en C#

Il y a quelques nuances à connaître lors de l'utilisation de l'écriture vers la sortie standard d'erreur en C#. Tout d'abord, la sortie standard d'erreur est différente de la sortie standard normale, qui est utilisée pour afficher les résultats de votre programme. La sortie standard d'erreur est souvent utilisée pour afficher des messages d'erreur, des avertissements ou des informations de débogage.

Deuxièmement, vous pouvez utiliser la classe Console.Error pour écrire directement vers la sortie standard d'erreur sans avoir à spécifier l'Error en utilisant Console.Out. Cela peut être utile si vous souhaitez écrire vers la sortie standard d'erreur à plusieurs endroits dans votre code.

Enfin, il est important de noter que vous pouvez également rediriger la sortie standard d'erreur vers un fichier texte en utilisant la commande '> ' lors de l'exécution de votre programme. Cela peut être utile si vous souhaitez enregistrer les messages d'erreur pendant que votre code s'exécute.

## Voir aussi

- [Documentation de Microsoft sur la classe Console.Error en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.console.error?view=net-5.0)
- [Article sur la redirection de la sortie dans Console en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/io/how-to-use-redirectstandarderror)
- [Guide de débogage en C# de Microsoft](https://docs.microsoft.com/fr-fr/dotnet/standard/exceptions/how-to-use-the-try-catch-block-to-catch-exceptions)