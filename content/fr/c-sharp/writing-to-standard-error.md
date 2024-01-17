---
title:                "Écrire vers l'erreur standard"
html_title:           "C#: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi & Comment?

Si vous êtes programmeur, vous avez probablement déjà rencontré la notion d'écrire dans l'erreur standard. En bref, c'est le moyen pour un programme de communiquer des messages d'erreur ou de débogage à l'utilisateur ou aux développeurs. C'est une pratique courante dans la programmation, car cela permet de repérer et de résoudre rapidement les problèmes dans le code.

# Comment faire:

Voici un exemple de code en C# pour écrire dans l'erreur standard:

```
Console.Error.WriteLine("Erreur: impossible d'ouvrir le fichier");
```

La sortie de ce code sera affichée dans la console en tant que message d'erreur. Vous pouvez également utiliser la méthode Console.SetError() pour définir une autre sortie pour l'erreur standard, telle qu'un fichier de journal.

```
Console.SetError(new StreamWriter("error_log.txt"));
```

# Exploration approfondie:

La pratique d'écrire dans l'erreur standard trouve ses racines dans le langage C, où un programmeur pouvait rediriger la sortie d'erreur pour la traiter séparément de la sortie standard. Dans d'autres langages, on peut utiliser des commandes telles que 'syslog' en C++ ou 'logging' en Python pour enregistrer ces messages d'erreur. Il est important de noter que l'écriture dans l'erreur standard ne doit pas être confondue avec les erreurs lancées par le code lui-même. Les messages d'erreur personnalisés doivent toujours être rédigés de manière appropriée.

# Voir aussi:

Voici quelques sources utiles pour en savoir plus sur l'écriture dans l'erreur standard:

- Microsoft: https://docs.microsoft.com/fr-fr/dotnet/api/system.console.seterror
- Stack Overflow: https://stackoverflow.com/questions/8877127/print-messages-to-the-console-not-stderror-in-c-sharp-console-applications