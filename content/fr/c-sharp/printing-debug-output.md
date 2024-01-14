---
title:                "C#: Affichage de sortie de débogage"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur de logiciel, il y a de fortes chances que vous ayez utilisé ou entendu parler de l'impression de sortie de débogage. Cette technique consiste à intégrer des instructions à votre code pour afficher des informations utiles lors de l'exécution du programme. Mais pourquoi est-il important de le faire ?

La principale raison d'utiliser l'impression de sortie de débogage est de faciliter le processus de débogage. En affichant des informations sur les variables et les étapes du code, vous pouvez rapidement identifier les erreurs et les corriger. Cela peut vous faire gagner un temps précieux lors du développement d'un programme.

## Comment Faire

Pour imprimer une sortie de débogage en C#, vous pouvez utiliser la méthode Console.WriteLine (). Cette méthode accepte une chaîne de caractères en argument et l'affiche dans la console lors de l'exécution du programme.

Voici un exemple de code montrant l'utilisation de Console.WriteLine () pour imprimer un message de débogage :

```C#
string message = "Hello, world !";
Console.WriteLine(message);
```

Lorsque vous exécutez ce code, vous verrez que la chaîne "Hello, world !" est affichée dans la console. C'est un simple exemple d'impression de la sortie de débogage, mais vous pouvez l'utiliser de différentes manières pour afficher des informations plus utiles.

Par exemple, vous pouvez utiliser des variables à la place de chaînes de caractères pour afficher des valeurs spécifiques dans la console. Ou vous pouvez utiliser Console.WriteLine () à différentes étapes de votre code pour suivre l'exécution et vérifier si toutes les variables ont les valeurs escomptées.

## Approfondissement

Il est important de garder à l'esprit que l'impression de sortie de débogage peut entraîner une baisse de performance de votre programme. Cela est dû au fait qu'il faut du temps pour afficher les informations dans la console, ce qui peut ralentir l'exécution de votre code.

Une autre chose à noter est que vous devriez éviter d'utiliser l'impression de sortie de débogage dans les versions finales de votre logiciel. Ce type de code peut être utile lors du développement, mais il n'est pas recommandé de laisser des instructions de débogage dans le code destiné aux utilisateurs finaux.

## Voir aussi

Si vous souhaitez en savoir plus sur l'utilisation de l'impression de sortie de débogage en C#, voici quelques ressources utiles :

- [Guide de débogage en C#](https://docs.microsoft.com/en-us/visualstudio/debugger/getting-started-with-the-debugger-csharp?view=vs-2019)
- [10 astuces de débogage en C#](https://blog.jetbrains.com/dotnet/2020/02/25/10-debugging-tips-csharp/)
- [Vidéo sur les techniques de débogage en C#](https://www.youtube.com/watch?v=TR8wcFAnUUM)

Maintenant que vous savez comment utiliser l'impression de sortie de débogage en C#, vous pouvez l'ajouter à votre boîte à outils de programmation pour vous aider à déboguer plus efficacement. Bonne codification !