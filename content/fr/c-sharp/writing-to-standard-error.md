---
title:                "Écrire sur l'erreur standard"
html_title:           "C#: Écrire sur l'erreur standard"
simple_title:         "Écrire sur l'erreur standard"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Pourquoi : Écrire vers l'erreur standard peut être une tâche utile pour les programmeurs lorsqu'ils doivent déboguer leur code et trouver des erreurs. Cela leur permet de voir les messages d'erreur en temps réel et d'identifier plus facilement les problèmes.

Comment Faire : Pour écrire vers l'erreur standard en C#, vous pouvez utiliser la méthode Console.Error.WriteLine(). Voici un exemple de code pour comprendre comment l'utiliser :

```C#
Console.Error.WriteLine("Erreur : la variable n'est pas définie.");
```

Cela imprimera le message d'erreur dans la console en rouge, ce qui le rendra plus facilement visible et identifiable. Voici à quoi cela ressemble dans la console :

![Exemple de sortie de l'erreur standard en C#](https://i.imgur.com/HmMsfga.png)

Vous pouvez également utiliser la méthode Console.Error.Write() pour écrire directement du texte à l'erreur standard sans retour à la ligne. Voici un exemple :

```C#
Console.Error.Write("Erreur critique :");
Console.Error.WriteLine(" Mémoire insuffisante");
```

Cela affichera "Erreur critique : Mémoire insuffisante" sur la même ligne dans la console.

Plongée Profonde : Écrire vers l'erreur standard peut également être utile pour les applications en direct lorsqu'il faut enregistrer des informations de débogage sans interrompre le fonctionnement normal de l'application. Vous pouvez utiliser la propriété Console.Error pour rediriger l'erreur standard vers un fichier de journalisation. Voici un exemple :

```C#
// Rediriger l'erreur standard vers un fichier journal
Console.SetError(new StreamWriter("log.txt"));
```

Cela écrira toutes les sorties de l'erreur standard dans le fichier "log.txt". Cela peut être particulièrement utile pour les applications en direct afin de garder un enregistrement des erreurs et des informations de débogage pour un dépannage ultérieur.

Voir Aussi : Pour en savoir plus sur la gestion des erreurs en C#, vous pouvez consulter les liens suivants :

- [Documentation officielle de Microsoft sur la gestion des erreurs en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/language-reference/keywords/try-catch)
- [Tutoriel sur les techniques de débogage en C#](https://www.tutorialspoint.com/csharp/csharp_debugging_techniques.htm)
- [Article sur la redirection de la sortie de la console en C#](https://www.codeproject.com/Articles/4696/Redirecting-the-Console-s-output-to-a-TextBox-in)