---
title:                "Vérifier l'existence d'un répertoire"
html_title:           "C#: Vérifier l'existence d'un répertoire"
simple_title:         "Vérifier l'existence d'un répertoire"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il serait utile de vérifier l'existence d'un répertoire en utilisant du code C#. Et bien, si vous êtes en train de développer une application qui nécessite l'utilisation de fichiers, une des premières choses à faire est de s'assurer que le répertoire dans lequel vous allez travailler existe bien. Ainsi, vous évitez les erreurs inattendues lors de l'exécution de votre code.

##Comment faire

Pour vérifier si un répertoire existe en utilisant C#, il suffit d'appeler la méthode `Directory.Exists()` en lui passant le chemin du répertoire en paramètre. Voici un exemple de code :

```C#
string chemin = @"C:\Users\Utilisateur\MesDocuments";
if (Directory.Exists(chemin))
{
    Console.WriteLine("Le répertoire existe !");
}
else
{
    Console.WriteLine("Le répertoire n'existe pas");
}
```

Si le répertoire existe, vous verrez le message "Le répertoire existe !" apparaître dans la console. Sinon, le message "Le répertoire n'existe pas" s'affichera.

## Plongeon en profondeur

Maintenant que vous savez comment vérifier l'existence d'un répertoire en utilisant C#, voici quelques informations supplémentaires qui pourraient vous être utiles :

- La méthode `Directory.Exists()` renvoie un booléen : `true` si le répertoire existe, `false` sinon.
- Si le répertoire donné en paramètre n'existe pas, la méthode `Directory.Exists()` renverra également `false`.
- Si vous souhaitez créer le répertoire si celui-ci n'existe pas, vous pouvez utiliser la méthode `Directory.CreateDirectory()` qui créera automatiquement le répertoire si celui-ci n'existe pas.

## Voir aussi

- [Documentation officielle de Microsoft sur la méthode Directory.Exists()](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.directory.exists?view=netcore-3.1)
- [Guide complet sur la gestion des fichiers et répertoires en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/file-system/)
- [Tutoriel sur la création de répertoires en C#](https://www.youtube.com/watch?v=nuBxZjkiJ7s) (vidéo en anglais)