---
title:    "Elixir: Création d'un fichier temporaire"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en Elixir?

La création de fichiers temporaires peut sembler peu utile, mais il y a en fait plusieurs raisons pour lesquelles cela peut être nécessaire dans un programme Elixir. Tout d'abord, cela peut être utilisé pour stocker temporairement des données pendant l'exécution du programme, sans avoir à les stocker dans une base de données ou à les transmettre à un autre processus. Deuxièmement, cela peut être utile lors de la manipulation de fichiers dans un système de fichiers, en créant un fichier temporaire pour stocker les données avant de les transférer dans un autre emplacement.

## Comment créer un fichier temporaire en Elixir?

Pour créer un fichier temporaire en Elixir, vous pouvez utiliser la fonction `Tempfile` du module `File` dans la bibliothèque standard d'Elixir. Cette fonction prend deux arguments obligatoires : le préfixe du nom de fichier et l'extension. Par exemple, pour créer un fichier temporaire avec le préfixe "temp" et l'extension ".txt", vous pouvez utiliser le code suivant :

```Elixir
file = File.tempfile("temp", ".txt")
```
La fonction renverra un tuple contenant deux valeurs : le chemin d'accès complet du fichier temporaire et un gestionnaire de fichier qui peut être utilisé pour lire et écrire des données dans le fichier. Ensuite, vous pouvez utiliser des fonctions de manipulation de fichiers telles que `File.write` ou `File.read` pour écrire ou lire des données dans le fichier.

## Plongez plus profondément dans la création de fichiers temporaires

Il est important de noter que la création de fichiers temporaires peut avoir des implications au niveau de la sécurité. Il est donc recommandé de supprimer le fichier temporaire une fois qu'il n'est plus nécessaire. Vous pouvez le faire en utilisant la fonction `File.rm` pour supprimer le fichier en spécifiant son chemin d'accès complet.

De plus, si vous avez besoin de créer plusieurs fichiers temporaires, vous pouvez utiliser la fonction `File.tmpname` qui renverra un chemin d'accès unique pour chaque fichier temporaire créé.

## Voir aussi

- [Documentation de la fonction Tempfile](https://hexdocs.pm/elixir/File.html#tempfile/2)
- [Documentation de la fonction tmpname](https://hexdocs.pm/elixir/File.html#tmpname/1)
- [Article sur la sécurité des fichiers temporaires en Elixir](https://blog.appsignal.com/2019/09/24/elixir-alchemy-creating-and-managing-temporary-files.html#security-considerations)