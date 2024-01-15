---
title:                "La lecture d'un fichier texte"
html_title:           "C#: La lecture d'un fichier texte"
simple_title:         "La lecture d'un fichier texte"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous travaillez sur des projets de programmation, il est fréquent d'avoir besoin de lire des fichiers textes. Que ce soit pour récupérer des données ou pour effectuer des opérations sur celles-ci, il est important de savoir comment lire un fichier texte en utilisant le langage de programmation C#.

## Comment faire

La lecture d'un fichier texte en utilisant C# est assez simple et peut être réalisée en quelques lignes de code seulement. Tout d'abord, vous devez spécifier le chemin d'accès au fichier que vous souhaitez lire en utilisant la classe `StreamReader` et le constructeur `new` :

```C#
StreamReader monFichier = new StreamReader("chemin/vers/mon/fichier.txt");
```

Ensuite, vous pouvez utiliser la méthode `ReadLine()` pour lire chaque ligne du fichier et l'afficher à l'écran :

```C#
string ligne = monFichier.ReadLine();
Console.WriteLine(ligne);
```

Vous pouvez également utiliser la méthode `ReadToEnd()` pour lire l'intégralité du fichier et la stocker dans une variable de type `string` :

```C#
string contenu = monFichier.ReadToEnd();
```

N'oubliez pas de fermer le fichier après l'avoir lu en utilisant la méthode `Close()`. Voici un exemple complet de lecture d'un fichier texte et d'affichage de son contenu à l'écran :

```C#
// Ouvre le fichier
StreamReader monFichier = new StreamReader("chemin/vers/mon/fichier.txt");

// Lit chaque ligne et l'affiche à l'écran
string ligne = monFichier.ReadLine();
Console.WriteLine(ligne);

// Lit l'intégralité du fichier et le stocke dans une variable
string contenu = monFichier.ReadToEnd();

// Ferme le fichier
monFichier.Close();
```

## Deep Dive

Maintenant que vous savez comment lire un fichier texte en utilisant C#, il est important de comprendre comment fonctionne ce processus de lecture. Lorsque vous appelez la méthode `ReadLine()`, celle-ci lit une ligne à la fois jusqu'à arriver à la fin du fichier. Si vous souhaitez lire la ligne suivante, vous devrez à nouveau appeler la méthode `ReadLine()`. Il en va de même pour la méthode `ReadToEnd()`, qui lit l'intégralité du fichier en une seule fois.

Il est également possible de spécifier la taille du buffer de lecture en utilisant le constructeur de la classe `StreamReader` avec un deuxième argument. Cela peut être utile pour améliorer les performances de lecture de fichiers de grande taille.

## Voir aussi

Pour en apprendre davantage sur la manipulation de fichiers en utilisant C#, vous pouvez consulter ces ressources :

- [Documentation officielle de C# sur la classe `StreamReader`](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Tutorial vidéo sur la lecture de fichiers texte en C#](https://www.youtube.com/watch?v=q-SJm6HTyyU)
- [Exemple de projet GitHub pour lire un fichier texte en C#](https://github.com/kallewie/CSharp-Examples-ReadTextFile)