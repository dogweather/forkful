---
title:                "Gleam: Création d'un fichier temporaire."
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est une tâche courante dans le développement de logiciels. Elle permet de stocker temporairement des données avant de les utiliser ou de les supprimer. Dans cet article, nous allons discuter de la création de fichiers temporaires en utilisant le langage de programmation Gleam.

## Comment faire

Pour créer un fichier temporaire en utilisant Gleam, nous pouvons utiliser la fonction `Gleam.File.Temp.create/2` qui prend deux arguments : le nom du fichier et le contenu du fichier.

```Gleam
temp_file = Gleam.File.Temp.create("mon-fichier-temporaire.txt", "Contenu de mon fichier temporaire")
```

Cette fonction renvoie un tuple avec le chemin d'accès au fichier temporaire et un indicateur pour savoir s'il a été créé avec succès.

```Gleam
(path, created) = temp_file

if created {
  // Le fichier temporaire a été créé avec succès
} else {
  // Le fichier temporaire n'a pas pu être créé
}
```

## Plongée en profondeur

En utilisant la fonction `Gleam.File.Temp.create/2`, nous pouvons également spécifier des options pour personnaliser notre fichier temporaire. Par exemple, nous pouvons spécifier l'extension de fichier en utilisant `ext` et le répertoire de stockage en utilisant `dir`.

```Gleam
options = [ext: ".csv", dir: "/chemin/vers/mon/dossier/"]

temp_file = Gleam.File.Temp.create("mon-fichier-temporaire", "Contenu de mon fichier temporaire", options)
```

Nous pouvons également utiliser la fonction `Gleam.File.Temp.dir/0` pour obtenir le chemin d'accès au répertoire de stockage des fichiers temporaires.

```Gleam
temp_dir = Gleam.File.Temp.dir()

path = temp_dir ++ "/mon-fichier-temporaire.txt"
```

## Voir aussi

Pour en savoir plus sur la création de fichiers temporaires en utilisant le langage de programmation Gleam, vous pouvez consulter la documentation officielle : 
- [Fonction `Gleam.File.Temp.create/2`](https://gleam.run/documentation/stdlib/file/#create)
- [Fonction `Gleam.File.Temp.dir/0`](https://gleam.run/documentation/stdlib/file/#dir)
- [Gleam Stdliv File module](https://gleam.run/documentation/stdlib/file/)