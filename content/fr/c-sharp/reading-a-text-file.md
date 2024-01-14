---
title:    "C#: Lecture d'un fichier texte"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi

Lecture de fichiers texte peut sembler une tâche banale en programmation, mais c'est en fait une compétence très utile à maîtriser. En tant que programmeur, vous pourriez être amené à lire et à manipuler des fichiers texte pour des tâches telles que l'analyse de données ou la gestion de configurations. Dans cet article, nous allons explorer comment lire un fichier texte en utilisant C#.

## Comment le faire

Voici un exemple de code en C# pour lire un fichier texte :

```C#
// Ouverture du fichier en lecture 
FileStream stream = new FileStream("monfichier.txt", FileMode.Open);

// Créer un StreamReader pour lire le fichier
StreamReader reader = new StreamReader(stream);

// Lire le contenu du fichier et stocker dans une chaîne
string contenu = reader.ReadToEnd();

// Fermer le StreamReader et le FileStream
reader.Close();
stream.Close();
```

Le code ci-dessus utilise la classe `FileStream` pour ouvrir le fichier en mode lecture et ensuite utilise la classe `StreamReader` pour lire le contenu du fichier en tant que chaîne. N'oubliez pas de fermer le `StreamReader` et le `FileStream` après avoir terminé la lecture.

## Plongée en profondeur

Bien qu'il semble simple de lire un fichier texte, il y a quelques points importants à noter :

- Les chemins de fichiers doivent être manipulés avec précaution. Assurez-vous d'utiliser les bonnes méthodes pour créer et accéder aux fichiers, sinon votre code risque de rencontrer des problèmes.
- Il est important de gérer correctement les exceptions pour éviter les erreurs et les plantages.
- Lorsque vous travaillez avec des fichiers volumineux, il est recommandé d'utiliser la classe `File` de C# qui fournit des méthodes plus efficaces pour lire et écrire des fichiers texte.

## Voir aussi

- [Classe File en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.file?view=netcore-3.1)
- [Classe StreamReader en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Manipuler les fichiers texte en C#](https://www.tutorialsteacher.com/csharp/csharp-file-io)