---
title:                "C#: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est courant dans la programmation de devoir vérifier si un dossier existe avant d'y accéder ou d'y enregistrer des fichiers. Cela peut s'avérer utile dans de nombreux scénarios, tels que la création d'un nouveau dossier si celui-ci n'existe pas encore ou la vérification de l'existence d'un dossier avant de le supprimer.

## Comment faire

Pour vérifier si un dossier existe en utilisant le langage C#, nous pouvons utiliser la méthode `Directory.Exists` de la classe `Directory`. Cette méthode prend en paramètre le chemin complet du dossier que nous voulons vérifier et renvoie un booléen indiquant si le dossier existe ou non.

Voici un exemple de code montrant comment utiliser cette méthode :

```c#
if (Directory.Exists("chemin/vers/dossier"))
{
    Console.WriteLine("Le dossier existe !");
}
else
{
    Console.WriteLine("Le dossier n'existe pas !");
}
```

L'exemple ci-dessus montre également comment nous pouvons utiliser une déclaration `if/else` pour afficher un message différent en fonction du résultat de la vérification.

Voici la sortie que nous obtiendrons si le dossier existe :

```
Le dossier existe !
```

Et voici la sortie que nous obtiendrons si le dossier n'existe pas :

```
Le dossier n'existe pas !
```

## Plongée en profondeur

Lorsque nous utilisons la méthode `Directory.Exists`, il est important de noter qu'elle renvoie `false` si le dossier n'existe pas ou si le chemin fourni n'est pas valide. Elle renverra également `false` si nous n'avons pas la permission d'accéder au dossier.

Il est également possible d'utiliser la méthode `Directory.Exists` pour vérifier l'existence d'un fichier en fournissant son chemin complet en tant que paramètre. Cela peut être utile si nous avons besoin de vérifier si un fichier existe avant de le lire ou de l'écrire.

Enfin, pour éviter des erreurs potentielles, il est recommandé d'utiliser la méthode `Directory.Exists` avant de tenter d'accéder ou de traiter un dossier. Ainsi, nous pouvons nous assurer que le dossier existe avant de continuer à l'utiliser dans notre code.

## Voir aussi

- [Classe Directory dans la documentation Microsoft](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.directory?view=net-5.0)
- [Méthode Exists dans la documentation Microsoft](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.directory.exists?view=net-5.0)