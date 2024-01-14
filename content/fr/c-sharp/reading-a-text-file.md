---
title:    "C#: Lecture d'un fichier texte"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi lire un fichier texte en C# ?

Lire un fichier texte en C# peut être utile pour de nombreuses raisons telles que la manipulation de données, l'analyse de texte ou la création de rapport. Dans cet article, nous allons plonger dans le processus de lecture d'un fichier texte, étape par étape.

## Comment faire :

Tout d'abord, nous avons besoin d'un fichier texte à lire. Nous pouvons utiliser la classe `File` du namespace `System.IO` pour ouvrir et lire un fichier. Voici un exemple de code en C# :

```C#
using System;
using System.IO;

namespace LectureFichier
{
    class Program
    {
        static void Main(string[] args)
        {
            // Chemin du fichier texte à lire
            string chemin = "chemin/vers/mon/fichier.txt";

            // Lecture du fichier et stockage dans une variable
            string contenu = File.ReadAllText(chemin);

            // Affichage du contenu du fichier
            Console.WriteLine(contenu);

            // Maintenir une pause à la fin pour voir le résultat
            Console.ReadLine();
        }
    }
}
```

Lorsque nous exécutons ce code, nous pouvons voir le contenu de notre fichier texte s'afficher dans la console. Nous pouvons également utiliser différentes méthodes de la classe `File` comme `ReadAllLines()` pour stocker chaque ligne du fichier dans un tableau ou `ReadAllTextAsync()` pour lire le fichier de façon asynchrone.

## Plongée en profondeur :

Maintenant que nous savons comment lire un fichier texte en utilisant la classe `File`, il est intéressant de comprendre un peu plus en détails le processus de lecture. Lorsque nous appelons la méthode `ReadAllText()` par exemple, elle va créer un objet `StreamReader` qui va lire le fichier caractère par caractère et le stocker dans un buffer. Le résultat final est ensuite retourné sous forme de chaîne de caractères.

Il existe également d'autres méthodes pour lire un fichier texte en C# telles que `ReadLines()` qui retourne un `IEnumerable<string>` ou `OpenText()` qui retourne un objet `StreamReader`.

# Voir aussi :

- [Documentation officielle sur la classe File en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.file)
- [Tutoriel sur la lecture de fichier texte en C#](https://www.c-sharpcorner.com/blogs/reading-text-files-in-c-sharp1)
- [Guide complet sur la manipulation de fichiers en C#](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)