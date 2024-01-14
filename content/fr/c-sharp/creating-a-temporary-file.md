---
title:                "C#: Création d'un fichier temporaire"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en C#

Créer des fichiers temporaires peut sembler une étape inutile, mais c'est en fait une pratique courante dans la programmation en C#. Ces fichiers temporaires sont utiles pour stocker des données qui ne sont nécessaires que temporairement, sans encombrer l'espace de stockage permanent. Dans cet article, nous allons expliquer pourquoi et comment créer des fichiers temporaires en C#, ainsi qu'une plongée plus profonde dans le sujet.

## Comment faire

Pour créer un fichier temporaire en C#, vous pouvez utiliser la méthode `GetTempFileName()` de la classe `System.IO.Path`. Cette méthode va générer un nom de fichier aléatoire dans le répertoire temporaire de l'utilisateur, qui peut ensuite être utilisé pour créer le fichier à l'aide de la classe `System.IO.File`.

```
C#
string tempFileName = Path.GetTempFileName(); // Génère le nom du fichier temporaire
File.Create(tempFileName); // Crée le fichier temporaire
```

Vous pouvez également spécifier un répertoire spécifique en utilisant la méthode `GetTempPath()` de la classe `System.IO.Path`.

```
C#
string tempPath = Path.GetTempPath() + "MonFichierTemp"; // Spécifie un répertoire temporaire
File.Create(tempPath); // Crée le fichier temporaire dans le répertoire spécifié
```

Une fois que vous avez terminé d'utiliser le fichier temporaire, assurez-vous de le supprimer en utilisant la méthode `Delete()` de la classe `System.IO.File`.

```
C#
File.Delete(tempFileName); // Supprime le fichier temporaire
```

## Plongée en profondeur

Lors de la création d'un fichier temporaire en C#, il y a quelques choses à garder à l'esprit. Tout d'abord, il est important de noter que ces fichiers seront automatiquement supprimés lorsque l'application à l'origine du fichier se terminera. Cela peut être utile pour nettoyer les fichiers temporaires et éviter de laisser des fichiers inutiles sur le système.

Deuxièmement, il est important de s'assurer que les noms de fichiers temporaires sont uniques pour éviter les conflits lors de la création de plusieurs fichiers en même temps. La méthode `GetTempFileName()` garantit un nom de fichier unique, mais si vous créez votre propre nom de fichier, assurez-vous qu'il n'est pas déjà utilisé.

Enfin, assurez-vous de prendre en compte la sécurité lors de la création de fichiers temporaires, en particulier si les fichiers contiennent des données sensibles. Il est recommandé d'utiliser la bibliothèque `System.Security` pour sécuriser vos fichiers temporaires.

## Voir aussi

- [Documentation de la classe `System.IO.Path`](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.path?view=netframework-4.8)
- [Documentation de la classe `System.IO.File`](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.file?view=netframework-4.8)
- [Documentation de la bibliothèque `System.Security`](https://docs.microsoft.com/fr-fr/dotnet/api/system.security?view=netframework-4.8)