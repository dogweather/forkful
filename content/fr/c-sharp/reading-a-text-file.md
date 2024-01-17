---
title:                "Lecture d'un fichier texte"
html_title:           "C#: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi et comment lire un fichier texte en C#

## Quoi & Pourquoi?
Lire un fichier texte en programmation signifie extraire et afficher le contenu d'un fichier texte sur l'ordinateur. Les programmeurs le font souvent pour accéder à des données stockées dans des fichiers ou traiter des textes volumineux.

## Comment faire:
Voici un exemple simple en ```C#``` montrant comment lire un fichier texte et afficher son contenu:

```C#
// Créer une instance de StreamReader pour lire le fichier
StreamReader reader = new StreamReader("mon_fichier.txt");
// Lire et afficher chaque ligne du fichier
string line;
while((line = reader.ReadLine()) != null) 
{
    Console.WriteLine(line);
}
// Fermer le lecteur pour libérer les ressources 
reader.Close();
```

## Plongée profonde:
- Contexte historique: La méthode ```StreamReader``` a été introduite dans le Framework .NET 1.0.
- Alternatives: D'autres méthodes pour lire un fichier texte incluent l'utilisation de ```File.ReadAllText()``` ou ```File.ReadAllLines()```.
- Détails de mise en œuvre: Lorsque vous utilisez la méthode ```StreamReader```, assurez-vous de fermer le lecteur pour libérer les ressources après utilisation.

## Voir aussi:
- [Documentation Microsoft sur la classe StreamReader](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Lire et écrire des fichiers en C#](https://www.microsoft.com/fr-fr/learn/modules/csharp-read-write-to-file/6-introduction)
- [Tutorial vidéo sur la lecture de fichiers texte en C#](https://www.youtube.com/watch?v=TVsgSQ_8mzY)