---
title:    "C#: Création d'un fichier temporaire"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est une pratique courante en programmation C#, car elle permet de stocker temporairement des données ou de créer des sauvegardes d'un travail en cours. Elle est également utile pour gérer la mémoire et éviter les problèmes de fragmentation.

## Comment faire

Pour créer un fichier temporaire en C#, il suffit d'utiliser la méthode "Path.GetTempFileName()". Voici un exemple de code :

```C#
string path = Path.GetTempFileName();
Console.WriteLine("Chemin du fichier temporaire : " + path);
```

L'exemple ci-dessus créera un fichier temporaire avec une extension .tmp dans le dossier temporaire de l'utilisateur. Vous pouvez également spécifier un emplacement spécifique en passant le chemin en paramètre de la méthode.

## Plongée en profondeur

Lors de la création d'un fichier temporaire, il est important de prendre en compte certains facteurs pour une utilisation efficace. Tout d'abord, il est recommandé d'utiliser des noms de fichiers aléatoires pour éviter les conflits avec d'autres fichiers temporaires ou existants. Vous pouvez également définir des permissions spécifiques pour le fichier temporaire en utilisant la classe "FileSecurity". Enfin, n'oubliez pas de supprimer le fichier temporaire une fois que vous avez fini de l'utiliser en utilisant la méthode "File.Delete(path)".

## Voir aussi

- [Microsoft documentation sur la classe Path](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.path?view=netcore-3.1)
- [Guide de programmation C# de C-sharpcorner.com](https://www.c-sharpcorner.com/UploadFile/mahesh/temporary-files-in-C-Sharp/)
- [Article sur les bonnes pratiques concernant les fichiers temporaires en C#](https://www.infoq.com/fr/articles/tmp-files-dotnet/)