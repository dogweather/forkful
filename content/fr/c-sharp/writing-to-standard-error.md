---
title:                "C#: Ecriture vers l'erreur standard"
simple_title:         "Ecriture vers l'erreur standard"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire vers la sortie d'erreur standard (standard error) est un élément essentiel de la programmation en C#. Cela permet aux développeurs de détecter et de résoudre les erreurs plus rapidement, améliorant ainsi la qualité globale du code.

## Comment Faire

Il existe plusieurs façons d'écrire vers la sortie d'erreur standard en C#, mais la méthode la plus courante est d'utiliser la méthode Console.Error.WriteLine(). Tout d'abord, il faut s'assurer que l'on importe l'espace de nom "System" en haut du fichier C#.

Pour écrire un message d'erreur, on peut utiliser la syntaxe suivante:

```C#
Console.Error.WriteLine("Erreur: Ceci est un message d'erreur.");
```

On peut également utiliser des variables et des chaînes de caractères pour personnaliser le message d'erreur et afficher des informations pertinentes pour le débogage du code:

```C#
int age = 25;

Console.Error.WriteLine($"Erreur: L'âge {age} n'est pas valide.");
```

Ce qui affichera le message suivant dans la sortie d'erreur:

> Erreur: L'âge 25 n'est pas valide.

## Plongée En Profondeur

La méthode Console.Error.WriteLine() permet non seulement d'afficher des messages d'erreur, mais aussi d'autres types de données tels que des entiers, des booléens et des objets. Il est également possible d'utiliser Console.Error.Write() pour écrire sans saut de ligne.

Il est également important de noter que les messages envoyés vers la sortie d'erreur standard sont affichés en rouge dans la console, ce qui les rend plus faciles à repérer parmi les autres messages.

## Voir Aussi

- [Documentation Microsoft sur Console.Error.WriteLine()](https://docs.microsoft.com/fr-fr/dotnet/api/system.console.error.writeline)
- [Tutoriel sur le débogage en C# (en français)](https://openclassrooms.com/fr/courses/281893-apprenez-a-programmer-avec-c/281707-le-mode-debug)