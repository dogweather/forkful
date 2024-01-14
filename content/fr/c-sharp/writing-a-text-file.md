---
title:    "C#: Écrire un fichier texte."
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi

Ecrire un fichier texte peut sembler une tâche banale dans le monde de la programmation. Mais en réalité, cela peut être un élément clé dans la mise en place d'un système bien structuré pour stocker et gérer des données. Que vous soyez un débutant ou un développeur expérimenté, savoir écrire un fichier texte peut vous faciliter la vie dans de nombreux projets de programmation.

## Comment faire

Tout d'abord, il est important de comprendre les outils de base pour écrire un fichier texte en C#. Il existe différentes façons de créer un fichier texte, mais la méthode la plus simple consiste à utiliser la classe `FileStream` et les classes associées telles que `StreamWriter` et `StreamReader`. Voici un exemple de code pour créer un fichier texte et y écrire des données :

```C#
using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        // Crée un nouveau fichier texte
        FileStream fichier = new FileStream(@"C:\Users\Utilisateur\monfichier.txt", FileMode.Create);

        // Utilise StreamWriter pour écrire dans le fichier
        StreamWriter ecrire = new StreamWriter(fichier);

        // Ajoute une ligne de texte
        ecrire.WriteLine("Voici un exemple de texte dans mon fichier.");

        // Ferme le fichier
        ecrire.Close();
    }
}
```

Le résultat sera un fichier texte contenant une ligne avec le texte "Voici un exemple de texte dans mon fichier.". En utilisant la méthode `WriteLine`, vous pouvez facilement ajouter plusieurs lignes de texte dans votre fichier.

Vous pouvez également utiliser la classe `StreamReader` pour lire des données à partir d'un fichier texte. Voici un exemple de code pour lire le contenu d'un fichier texte ligne par ligne :

```C#
// Définit le chemin d'un fichier texte
string chemin = @"C:\Users\Utilisateur\monfichier.txt";

// Utilise StreamReader pour lire le fichier
StreamReader lire = new StreamReader(chemin);

// Lit et affiche une ligne à la fois
string ligne;
while ((ligne = lire.ReadLine()) != null)
{
    Console.WriteLine(ligne);
}

// Ferme le fichier
lire.Close();
```

## Plongée en profondeur

Maintenant que vous savez comment créer et lire un fichier texte en C#, vous pouvez également approfondir vos connaissances en apprenant d'autres fonctionnalités utiles telles que la lecture et l'écriture de données structurées, la gestion des erreurs et la manipulation de fichiers de grande taille.

La classe `File` offre des méthodes pratiques pour vérifier si un fichier existe, le supprimer ou renommer. Vous pouvez également utiliser la classe `StringBuilder` pour manipuler des chaînes de caractères plus complexes avant de les écrire dans un fichier.

Ne vous contentez pas d'écrire des fichiers texte simples, expérimentez et découvrez les nombreuses possibilités que C# offre pour la gestion de fichiers texte.

## Voir aussi

- [Documentation Microsoft sur les opérations de fichiers en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/io/)
- [Les fonctions de gestion de fichiers en C# sur GeeksforGeeks](https://www.geeksforgeeks.org/file-management-in-c-sharp/)
- [Tutoriel YouTube sur la création et la lecture de fichiers texte en C#](https://www.youtube.com/watch?v=kTSsZg-eWqA)