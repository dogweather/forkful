---
title:                "Écrire un fichier texte"
html_title:           "C#: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte peut sembler une tâche simple et banale, mais c'est en réalité une compétence fondamentale pour tout programmeur en C#. Les fichiers texte sont essentiels pour stocker et manipuler des données, et ils offrent une solution pratique pour communiquer avec d'autres programmes et utilisateurs.

## Comment faire

Pour écrire un fichier texte en C#, il existe plusieurs étapes à suivre :

1. Définir un chemin d'accès et un nom de fichier pour votre fichier texte. Cela peut être un chemin absolu (comme "C:\monFichier.txt") ou relatif (comme "monDossier\monFichier.txt").
2. Créer un objet de type StreamWriter en utilisant le chemin d'accès et le nom de fichier que vous avez définis.
3. Utiliser la méthode StreamWriter.Write ou StreamWriter.WriteLine pour écrire le contenu de votre fichier texte dans l'objet StreamWriter.
4. N'oubliez pas de toujours fermer votre objet StreamWriter en utilisant la méthode Close pour enregistrer les modifications apportées à votre fichier texte.

Un exemple de code pour écrire un fichier texte en utilisant ces étapes serait le suivant :

```C#
// Définition du chemin d'accès et du nom du fichier
string chemin = "C:\monFichier.txt";

// Création de l'objet StreamWriter
StreamWriter writer = new StreamWriter(chemin);

// Utilisation des méthodes Write et WriteLine pour écrire du contenu dans l'objet StreamWriter
writer.WriteLine("Ceci est une première ligne dans mon fichier texte.");
writer.Write("Et voici une deuxième ligne.");

// Fermeture de l'objet StreamWriter
writer.Close();
```

Lorsque vous exécutez ce code, un fichier texte nommé "monFichier.txt" sera créé dans le chemin d'accès spécifié, et contiendra les deux lignes que vous avez écrites.

## Plongée en profondeur

En utilisant le concept de flux (stream) de données, l'objet StreamWriter vous permet d'écrire des données dans un fichier de manière séquentielle. Cela signifie que vous pouvez ajouter du contenu à votre fichier en utilisant plusieurs appels aux méthodes Write ou WriteLine, plutôt que d'avoir à tout écrire en une seule fois.

En plus de cela, l'objet StreamWriter offre également des options pour spécifier l'encodage du fichier (UTF-8, ASCII, etc.), gérer les exceptions et vérifier l'état d'avancement de l'écriture dans le fichier.

## Voir aussi

- [Guide de référence Microsoft pour StreamWriter](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter?view=net-5.0)
- [Exemple de code pour écrire un fichier texte en C#](https://www.c-sharpcorner.com/code/3904/export-csv-excel-in-c-sharp-using-ado-net-data-reader) (article en anglais)
- [Utilisation des fichiers texte en C#](https://www.ict.social/c-sharp/files-and-streams-in-csharp) (article en anglais)