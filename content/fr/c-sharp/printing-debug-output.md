---
title:                "C#: Affichage des sorties de débogage"
simple_title:         "Affichage des sorties de débogage"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

Pourquoi imprimer des sorties de débogage ?

Imprimer des sorties de débogage est un outil précieux pour les développeurs lorsqu'ils tentent de résoudre les problèmes de leur code. Cela leur permet de voir les résultats de différentes étapes du code et de déterminer où se trouvent les erreurs.

Comment faire :

```c#
Console.WriteLine("Bonjour le monde !"); // Imprime le texte "Bonjour le monde !" dans la console
```

La console est l'outil principal pour imprimer des sorties de débogage en C#. Vous pouvez utiliser la méthode `Console.WriteLine()` pour afficher du texte dans la console. Vous pouvez également utiliser des instructions de débogage telles que `Debug.WriteLine()` ou `Trace.WriteLine()` pour imprimer des informations supplémentaires.

Dans l'exemple ci-dessus, nous avons utilisé `Console.WriteLine()` pour imprimer le texte "Bonjour le monde !" dans la console. Vous pouvez également utiliser des variables ou des expressions à la place du texte pour afficher des données spécifiques à une partie de votre code. Par exemple :

```c#
int nombre = 5;
Console.WriteLine("Le nombre est : " + nombre); // Imprime "Le nombre est : 5" dans la console
```

Vous pouvez également utiliser des `if` ou des `for` boucles pour imprimer des résultats spécifiques en fonction de certaines conditions.

Deep Dive :

Bien qu'imprimer des sorties de débogage puisse sembler simple, il existe en réalité de nombreuses façons différentes de les utiliser. Vous pouvez utiliser des bibliothèques de log comme NLog ou Serilog pour enregistrer des sorties de débogage dans des fichiers ou des bases de données. Vous pouvez également utiliser des attributs de débogage pour activer ou désactiver les sorties de débogage en fonction de la configuration de votre application.

L'utilisation d'impressions de débogage peut également être utile lorsque vous travaillez avec du code asynchrone ou du multithreading, car cela peut vous aider à suivre le flux d'exécution du code.

N'oubliez pas que l'impression de sorties de débogage ne doit pas être utilisée en tant que méthode principale pour résoudre les problèmes de code. C'est plutôt un outil complémentaire qui peut vous aider à comprendre comment votre code s'exécute.

Voir aussi :

- [Guide complet pour l'utilisation des sorties de débogage en C#](https://docs.microsoft.com/fr-fr/dotnet/core/diagnostics/logging)
- [Documentation officielle de la classe Console en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.console?view=netframework-4.8)
- [Bibliothèque de log NLog](https://nlog-project.org/)
- [Bibliothèque de log Serilog](https://serilog.net/)