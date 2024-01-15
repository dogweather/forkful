---
title:                "Création d'un fichier temporaire"
html_title:           "C#: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en C#?

Il peut être utile de créer un fichier temporaire en C# dans certaines situations lors du développement de logiciels. Par exemple, cela peut être nécessaire pour stocker des données temporaires ou pour exécuter des tests.

## Comment faire

Pour créer un fichier temporaire en C#, vous pouvez utiliser la classe `Path` dans l'espace de noms `System.IO`. Vous devrez également utiliser la méthode `GetTempFileName()` qui renvoie le chemin d'accès au fichier temporaire créé. Voici un exemple de code :

```C#
//include la déclaration de namespace suivante
using System.IO;

//créer un fichier temporaire
string tempFile = Path.GetTempFileName();

//écrire des données dans le fichier temporaire
File.WriteAllText(tempFile, "Ceci est un exemple de données.");

//lire les données du fichier temporaire
Console.WriteLine(File.ReadAllText(tempFile));

//supprimer le fichier temporaire
File.Delete(tempFile);
```

Lorsque vous exécutez ce code, vous devriez voir le texte "Ceci est un exemple de données." apparaître dans la console.

## Deep Dive

Il est important de noter que la méthode `GetTempFileName()` crée automatiquement un fichier temporaire vide. Il vous suffit donc simplement d'écrire ou de lire des données dans ce fichier. De plus, le chemin d'accès renvoyé par la méthode est unique à chaque appel, ce qui garantit que vous ne créerez pas accidentellement plusieurs copies du même fichier temporaire.

## Voir aussi

- [Documentation officielle de la classe Path en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.path?view=netcore-3.1)
- [Documentation officielle de la méthode GetTempFileName() en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.path.gettempfilename?view=netcore-3.1)