---
title:                "C#: Lecture d'un fichier texte"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi lire un fichier texte?

Lire un fichier texte en C# est une pratique courante pour de nombreux programmeurs. Cela leur permet de stocker et de traiter de grandes quantités de données de manière organisée. Dans cet article, nous allons vous montrer comment lire un fichier texte en C# et pourquoi c'est une compétence essentielle pour tout programmeur.

## Comment faire pour lire un fichier texte en C#?

Pour lire un fichier texte en C#, vous devez suivre ces étapes:

1. Tout d'abord, nous devons créer un objet de type `StreamReader` en utilisant la classe `StreamReader` disponible dans l'espace de noms `System.IO`.
2. Ensuite, nous devons spécifier le chemin d'accès complet du fichier texte que nous voulons lire.
3. Nous utilisons ensuite la méthode `ReadLine` pour lire chaque ligne du fichier texte jusqu'à ce que nous atteignions la fin du fichier.
4. Enfin, nous utilisons la méthode `Close` pour fermer le `StreamReader` et libérer les ressources utilisées.

Voici un exemple de code pour lire un fichier texte en C#:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Créer un objet StreamReader pour lire le fichier texte.
        StreamReader sr = new StreamReader("monFichier.txt");

        // Lire lignes par lignes jusqu'à la fin du fichier.
        string ligne;
        while ((ligne = sr.ReadLine()) != null)
        {
            Console.WriteLine(ligne);
        }

        // Fermer le StreamReader pour libérer les ressources.
        sr.Close();
    }
}
```

Lorsque vous exécutez ce code, vous devriez voir le contenu de votre fichier texte s'afficher dans la console.

## Plongée Profonde

Maintenant que vous savez comment lire un fichier texte en C#, vous pouvez également utiliser d'autres méthodes pour lire et traiter les données. Par exemple, la méthode `Read` vous permet de lire un certain nombre de caractères à la fois, tandis que la méthode `ReadToEnd` vous permet de lire tout le contenu du fichier en une seule fois.

De plus, vous pouvez également utiliser des boucles et des conditions pour effectuer des opérations spécifiques sur les données lues à partir du fichier. N'hésitez pas à explorer et à expérimenter avec ces différentes méthodes pour trouver celle qui convient le mieux à votre projet.

# Voir Aussi
- [Documentation de Microsoft sur la classe StreamReader](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.streamreader)
- [Tutorial sur la lecture de fichiers en C#](https://www.tutorialspoint.com/csharp/csharp_text_files.htm)
- [Vidéo YouTube sur la lecture de fichiers texte en C#](https://www.youtube.com/watch?v=dF0ZPruKoJU)