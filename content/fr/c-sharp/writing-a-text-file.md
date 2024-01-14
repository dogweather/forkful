---
title:    "C#: Écrire un fichier texte"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

Pourquoi écrire un fichier texte en C# ?

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir écrire un fichier texte en utilisant C#. Tout d'abord, cela peut être utile pour stocker des données importantes, telles que des noms d'utilisateur et des mots de passe dans un fichier sécurisé. Deuxièmement, cela peut également être utile pour créer des rapports ou des journaux de données, qui peuvent être facilement lus et analysés par d'autres programmes.

Comment écrire un fichier texte en C# ?

Pour écrire un fichier texte en utilisant C#, vous pouvez suivre ces étapes simples :

1. Tout d'abord, importez la classe System.IO pour pouvoir accéder aux fonctionnalités de lecture et d'écriture de fichiers.

2. Ensuite, déclarez une variable de type StreamWriter qui sera utilisée pour écrire des données dans le fichier. Vous pouvez spécifier le chemin du fichier en utilisant le constructeur de la classe.

3. Utilisez la méthode Write ou WriteLine pour écrire des données dans le fichier. Ces méthodes prennent une chaîne de caractères en paramètre.

Voici un exemple complet de code qui montre comment écrire dans un fichier texte en utilisant C# :

```C#
using System.IO;

class Program
{
  static void Main(string[] args)
  {
    // Ouvre le fichier texte en mode écriture
    using (StreamWriter writer = new StreamWriter(@"C:\monfichier.txt"))
    {
      // Écrit une ligne dans le fichier
      writer.WriteLine("Bonjour le monde !");
      // Écrit du texte sans saut de ligne
      writer.Write("Ceci est un ");
      writer.Write("exemple ");
      writer.Write("de fichier texte.");
    }
  }
}
```

Et voici le contenu de "monfichier.txt" après l'exécution du programme :

```
Bonjour le monde !
Ceci est un exemple de fichier texte.
```

Plongée en profondeur

Maintenant, plongeons un peu plus en détails sur l'utilisation de la classe StreamWriter pour écrire dans un fichier texte. Cette classe dispose de plusieurs méthodes qui peuvent être utiles en fonction de vos besoins :

- Write(char) : Cette méthode écrit un seul caractère dans le fichier.
- Write(char[]) : Écrit un tableau de caractères dans le fichier.
- Write(double) : Écrit un nombre à virgule flottante dans le fichier.
- Write(int), Write(uint), Write(long), Write(ulong), Write(decimal) : Écrit un nombre entier dans le fichier. Chaque méthode accepte également une chaîne de format en paramètre pour spécifier la mise en forme du nombre.
- Flush() : Cette méthode force le vidage du tampon et l'écriture de toutes les données qui y sont stockées dans le fichier.

Pour une liste complète des méthodes disponibles, vous pouvez consulter la documentation officielle de Microsoft sur la classe StreamWriter.

Voir aussi

- [Guide de démarrage avec C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/)

- [Tutoriel sur la classe System.IO pour la manipulation de fichiers en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file)

- [Documentation officielle de Microsoft sur la classe StreamWriter](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.streamwriter)