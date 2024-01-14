---
title:    "C#: Vérifier si un répertoire existe"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà eu besoin de vérifier si un répertoire existe dans votre programme C#? Peut-être que vous souhaitez vous assurer qu'un chemin d'accès est valide avant d'y accéder ou peut-être que vous voulez créer un nouveau répertoire si celui-ci n'existe pas encore. Quelle que soit la raison, il est important de savoir comment vérifier efficacement si un répertoire existe.

## Comment Faire

Pour vérifier si un répertoire existe dans un programme C#, nous utiliserons la classe Directory de l'espace de noms System.IO. Cette classe fournit des méthodes pratiques pour manipuler les répertoires et les fichiers.

Commençons par déclarer une variable qui contiendra le chemin d'accès que nous voulons vérifier :

```C#
string chemin = @"C:\Users\Utilisateur\Documents\NouveauDossier";
```

Ensuite, nous pouvons utiliser la méthode statique Exists de la classe Directory pour vérifier si le répertoire existe :

```C#
if(Directory.Exists(chemin))
{
    Console.WriteLine("Le répertoire existe !");
}
else
{
    Console.WriteLine("Le répertoire n'existe pas.");
}
```

Dans cet exemple, nous vérifions si le répertoire "NouveauDossier" existe dans le chemin d'accès spécifié. Si c'est le cas, alors nous affichons un message indiquant que le répertoire existe. Sinon, nous affichons un message indiquant qu'il n'existe pas.

## Profonde Plongée

Lorsque nous utilisons la méthode Exists, nous avons simplement besoin de lui passer le chemin d'accès du répertoire que nous voulons vérifier. Cependant, il existe d'autres méthodes de la classe Directory qui peuvent être utiles dans certains scénarios :

- CreateDirectory : Cette méthode créera un nouveau répertoire s'il n'existe pas déjà à l'emplacement spécifié.
- Delete : Cette méthode supprimera un répertoire spécifié.
- GetCreationTime : Cette méthode renvoie la date de création d'un répertoire spécifié.
- GetFiles, GetDirectories : Ces méthodes renvoient une liste des fichiers ou répertoires contenus dans un répertoire spécifique.

Maintenant que vous savez comment vérifier si un répertoire existe dans un programme C#, vous pouvez l'implémenter dans vos projets pour une meilleure gestion des répertoires et des fichiers.

## Voir Aussi

- [Documentation officielle de la classe Directory en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.directory)
- [Guide complet sur la manipulation des fichiers et des répertoires en C#](https://www.codegrepper.com/code-examples/csharp/c%23+create+directory)
- [Tutoriel vidéo sur la manipulation des répertoires en C#](https://www.youtube.com/watch?v=IjzGJBFsMIQ)