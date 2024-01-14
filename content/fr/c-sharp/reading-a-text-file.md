---
title:                "C#: Lire un fichier texte"
simple_title:         "Lire un fichier texte"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

Pourquoi: La lecture de fichiers texte est une compétence essentielle pour tout programmeur en C#. Elle permet de manipuler des données stockées dans des fichiers, ce qui est souvent nécessaire pour les applications de traitement de données ou de conversion de données.

Comment faire: Voici un exemple de code en C# qui montre comment lire un fichier texte et afficher son contenu:

```C#
// Ouvrir le fichier en utilisant File.OpenText()
string cheminFichier = @"C:\MonFichier.txt";
StreamReader fichier = File.OpenText(cheminFichier);

// Lire et afficher chaque ligne du fichier
string ligne;
while((ligne = fichier.ReadLine()) != null)
{
    Console.WriteLine(ligne);
}

// Fermer le fichier
fichier.Close();
```

Output:
```
Première ligne du fichier
Deuxième ligne du fichier
Troisième ligne du fichier
```

Plonger plus profondément: La classe `StreamReader` utilisée dans l'exemple ci-dessus est une classe de System.IO qui permet de lire des fichiers texte de manière efficace. Elle dispose de nombreuses méthodes utiles pour l'exploration de fichiers, telles que `Read`, `Peek` et `ReadToEnd`. En utilisant ces méthodes et d'autres fonctionnalités de la classe, vous pouvez personnaliser votre façon de lire des fichiers texte.

Voir aussi:
- [Documentation officielle de la classe StreamReader en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.streamreader?view=net-5.0)
- [Tutorial: Lecture et écriture de fichiers en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file)
- [Exemple de lecture de fichier texte en C#](https://www.c-sharpcorner.com/UploadFile/mahesh/read-tex-from-a-file-in-C-Sharp/)