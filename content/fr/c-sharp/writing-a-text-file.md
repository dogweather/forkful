---
title:                "C#: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte?

Les fichiers texte sont utilisés dans de nombreux programmes informatiques pour stocker des données. En écrivant un fichier texte, vous pouvez créer un enregistrement permanent de vos données ou de vos résultats de programme pour une utilisation future. Cela peut être particulièrement utile pour la sauvegarde ou le partage de données importantes.

## Comment écrire un fichier texte en C#?

L'écriture d'un fichier texte en C# est assez simple grâce à la classe `StreamWriter`. Tout d'abord, vous devrez ajouter `using System.IO;` en haut de votre code pour utiliser cette classe. Ensuite, vous devrez déclarer une instance de `StreamWriter` en utilisant le chemin de votre fichier texte comme paramètre. Enfin, vous pouvez écrire du contenu dans le fichier texte à l'aide de la méthode `Write` ou `WriteLine`.

Voici un exemple de code en C# pour écrire un fichier texte:

```C#
using System;
using System.IO;

class Program
{
  static void Main()
  {
    //Déclaration de l'instance StreamWriter
    StreamWriter sw = new StreamWriter("monTexte.txt");

    //Ecriture de contenu dans le fichier texte
    sw.WriteLine("Bonjour!");
    sw.WriteLine("Voici un exemple de fichier texte en C#.");
    sw.Close(); //N'oubliez pas de fermer le StreamWriter après l'utilisation
    
    //Vérification du contenu du fichier texte en utilisant un StreamReader
    StreamReader sr = new StreamReader("monTexte.txt");
    Console.WriteLine(sr.ReadToEnd()); //Ecrit le contenu du fichier dans la console
    sr.Close(); //Ferme le StreamReader après utilisation
  }
}
```

Dans cet exemple, nous avons créé un fichier texte appelé "monTexte.txt" et avons écrit deux lignes de contenu à l'aide de la méthode `WriteLine`. En utilisant un `StreamReader`, nous avons ensuite vérifié que notre texte a été correctement écrit dans le fichier en le lisant et en l'affichant dans la console.

## Plongeon en profondeur: écrire et formater un fichier texte

En plus d'écrire du contenu simple dans un fichier texte, vous pouvez également le formater en utilisant des caractères spéciaux et des méthodes de formatage de chaîne. La méthode `WriteLine` accepte un paramètre optionnel de formatage qui peut être utilisé pour contrôler l'alignement, la largeur et la précision de la valeur écrite.

Voici un exemple de code qui utilise les méthodes de formatage pour écrire un tableau de valeurs dans un fichier texte:

```C#
using System;
using System.IO;

class Program
{
  static void Main()
  {
    //Déclaration de l'instance StreamWriter
    StreamWriter sw = new StreamWriter("monTableau.txt");

    //Ecriture d'un tableau de valeurs
    sw.WriteLine("Nombre 1: {0,-10}Nombre 2: {1,-10}Nombre 3: {2,-10}", 10, 20.5, 30.789);
    sw.Close(); //Ferme le StreamWriter après utilisation
  }
}
```

Dans cet exemple, nous utilisons les paramètres `{0,-10}`, `{1,-10}` et `{2,-10}` qui indiquent respectivement que la première, deuxième et troisième valeur écrite doivent être alignées à gauche avec une largeur de 10 caractères.

## Voir aussi:
- [Documentation Microsoft pour la classe StreamWriter en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.streamwriter?view=netcore-3.1)
- [Tutoriel vidéo pour écrire et lire des fichiers texte en C#](https://www.youtube.com/watch?v=6-lPW191z8Y)