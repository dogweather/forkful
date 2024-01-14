---
title:                "C#: Vérification de l'existence d'un répertoire"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des questions les plus courantes lors de la programmation est de savoir si un répertoire existe déjà ou non. Cela peut sembler être une tâche simple, mais la vérification de l'existence d'un répertoire peut être utile dans divers scénarios de programmation pour garantir que les fichiers sont bien organisés et accessibles.

## Comment faire

Pour vérifier si un répertoire existe en utilisant C#, nous pouvons utiliser la méthode "Directory.Exists" qui renvoie un booléen indiquant si le répertoire spécifié existe ou non.

```C#
if (Directory.Exists("chemin/du/répertoire"))
{
    Console.WriteLine("Le répertoire existe !");
}
else
{
    Console.WriteLine("Le répertoire n'existe pas !");
}
```

Output:

```console
Le répertoire existe !
```

## Plongée en profondeur

La méthode "Directory.Exists" effectue simplement une vérification au niveau de l'interface utilisateur pour déterminer si le répertoire existe. Cela signifie qu'elle n'est pas entièrement fiable, car un répertoire peut être créé ou supprimé pendant l'exécution du programme.

Pour une vérification plus précise, nous pouvons utiliser la classe "DirectoryInfo" qui fournit des méthodes pour effectuer des opérations de bas niveau sur les répertoires. Par exemple, nous pouvons utiliser la méthode "Exists" pour vérifier l'existence d'un répertoire et "Create" pour créer un répertoire si celui-ci n'existe pas encore.

## Voir aussi

- Documentation sur la méthode "Directory.Exists" : https://docs.microsoft.com/fr-fr/dotnet/api/system.io.directory.exists
- Documentation sur la classe "DirectoryInfo" : https://docs.microsoft.com/fr-fr/dotnet/api/system.io.directoryinfo
- Tutoriel de Microsoft sur la gestion des répertoires en C# : https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/file-system/how-to-iterate-through-a-directory-tree