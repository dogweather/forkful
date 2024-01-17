---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "C#: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La vérification de l'existence d'un répertoire est une opération courante pour les programmeurs en C#. Il s'agit simplement de vérifier si un répertoire existe avant de tenter de l'utiliser dans votre code. Cela peut être utile pour éviter les erreurs lors de l'accès à un répertoire qui n'existe pas.

## Comment faire:

Il existe plusieurs façons de vérifier si un répertoire existe en C#. La plus simple consiste à utiliser la méthode `Directory.Exists()` de la classe `System.IO`, qui renvoie un booléen indiquant si le répertoire existe ou non. Voici un exemple de code montrant comment utiliser cette méthode:

```C#
if (Directory.Exists("chemin/vers/mon/répertoire"))
{
    Console.WriteLine("Le répertoire existe.");
}
else
{
    Console.WriteLine("Le répertoire n'existe pas.");
}
```

Cela affichera "Le répertoire existe." si le répertoire existe effectivement, ou "Le répertoire n'existe pas." s'il n'existe pas.

## Plongeons plus profondément:

Avant la version 4.0 de .NET Framework, la méthode `Directory.Exists()` utilisait la méthode `GetFileAttributes()` de l'API Windows pour déterminer si un répertoire existait. Cependant, cette méthode n'était pas fiable dans certaines situations, comme lorsque l'utilisateur n'a pas les autorisations nécessaires pour accéder au répertoire. Depuis la version 4.0, la méthode utilise la méthode `FindFirstFile()` du système de fichiers Windows, ce qui est plus fiable.

Une alternative à l'utilisation de `Directory.Exists()` est d'utiliser la méthode `Directory.EnumerateDirectories()` pour obtenir une liste de tous les répertoires dans un chemin spécifique, puis de vérifier si le répertoire recherché est présent dans cette liste.

## Voir aussi:

- [La documentation officielle sur la méthode Directory.Exists()](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.directory.exists)
- [Un article de blog sur les différentes façons de vérifier l'existence d'un répertoire en C#](https://www.c-sharpcorner.com/article/how-to-check-if-a-directory-exists-in-C-Sharp/)