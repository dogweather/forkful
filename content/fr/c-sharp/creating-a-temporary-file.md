---
title:    "C#: Créer un fichier temporaire"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi?

L'utilisation de fichiers temporaires peut sembler un concept assez simple, mais il peut être très utile dans de nombreuses applications. Les fichiers temporaires sont des fichiers créés pour une courte période de temps, dans le but de stocker des données temporaires ou de servir d'emplacement pour les processus en cours d'exécution. Dans cet article, nous allons plonger dans la création et l'utilisation de fichiers temporaires en utilisant C#.

## Comment procéder?

Pour créer un fichier temporaire en C#, nous utiliserons la classe "Path" et sa méthode "GetTempFileName". Cette méthode génère un nom de fichier unique et le place dans le répertoire temporaire du système. Nous pouvons ensuite utiliser ce nom pour créer notre fichier temporaire en utilisant la classe "StreamWriter". Voici un exemple de code simple pour créer et écrire dans un fichier temporaire:

```C#
string nomFichier = Path.GetTempFileName();
using(StreamWriter sw = File.AppendText(nomFichier))
{
    sw.WriteLine("Contenu du fichier temporaire");
}
```

Une fois le fichier temporaire créé, nous pouvons continuer à travailler avec lui comme tout autre fichier. Lorsque nous avons terminé d'utiliser le fichier, nous pouvons le supprimer en utilisant la méthode "File.Delete". Il est important de noter que les fichiers temporaires sont supprimés automatiquement lorsque l'application se termine, mais il est toujours bon de les supprimer manuellement pour éviter d'encombrer le système de fichiers.

Dans l'exemple ci-dessus, nous avons utilisé la classe "StreamWriter" pour écrire dans le fichier temporaire. Mais il est également possible d'utiliser la classe "File" pour copier un fichier existant vers un fichier temporaire. Voici un exemple:

```C#
string cheminFichier = @"C:\Users\nomUtilisateur\existant.txt";
string nomFichierTemp = Path.GetTempFileName();
File.Copy(cheminFichier, nomFichierTemp);
```

## Plongée en profondeur

En regardant de plus près le processus de création de fichiers temporaires en C#, il est important de noter que la classe "Path" utilise le répertoire temporaire du système pour créer ces fichiers. Ce répertoire peut varier en fonction du système d'exploitation et des paramètres de l'utilisateur. Il est recommandé d'utiliser la méthode "GetTempPath" de la classe "Path" pour obtenir le chemin du répertoire temporaire avant de créer un fichier temporaire.

De plus, il est possible de spécifier un répertoire spécifique pour stocker les fichiers temporaires en utilisant la méthode "GetTempFileName" avec un paramètre supplémentaire spécifiant le répertoire souhaité.

## Voir aussi

Pour en savoir plus sur la gestion des fichiers en C#, vous pouvez consulter les ressources suivantes:

- [Documentation Microsoft pour la classe "Path"](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.path?view=net-5.0)
- [Documentation Microsoft pour la classe "File"](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.file?view=net-5.0)
- [Tutorial sur la gestion des fichiers en C#](https://www.youtube.com/watch?v=89lz0zsCogU)