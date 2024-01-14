---
title:                "Gleam: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous développez des applications ou des scripts en Gleam, il peut souvent être nécessaire de créer des fichiers temporaires pour stocker des données pendant un certain temps. Les fichiers temporaires sont utiles pour stocker des informations temporaires, telles que des variables ou des résultats intermédiaires, et peuvent être supprimés une fois qu'ils ne sont plus nécessaires. Dans cet article, nous allons expliquer pourquoi et comment créer des fichiers temporaires en Gleam.

## Comment Faire

Pour créer un fichier temporaire en Gleam, nous allons utiliser la fonction `Temp.file`, qui prend deux arguments en entrée : le préfixe du nom de fichier et le suffixe. Le préfixe est utilisé pour créer un nom de fichier unique, tandis que le suffixe détermine le type de fichier que nous allons créer. Voici un exemple de code pour créer un fichier temporaire avec le préfixe "temp" et le suffixe ".txt" :

```Gleam
let temp_file = Temp.file("temp", ".txt")
```

Cela va créer un fichier de la forme "temp-12345.txt", où "12345" est un nombre aléatoire généré automatiquement pour créer un nom unique. Ensuite, nous pouvons écrire du contenu dans notre fichier temporaire en utilisant la fonction `File.write` :

```Gleam
File.write(temp_file, "Contenu du fichier")
```

Nous pouvons également lire le contenu du fichier en utilisant la fonction `File.read` et afficher le résultat dans la console :

```Gleam
let content = File.read(temp_file)
IO.println(content)
```

Et voilà ! Nous avons maintenant créé et écrit dans un fichier temporaire en utilisant Gleam.

## Plongée Profonde

En plongeant un peu plus dans la création de fichiers temporaires en Gleam, nous pouvons voir que la fonction `Temp.file` utilise en fait le module `Temp` pour générer un nom de fichier unique en utilisant l'horodatage en millisecondes et le PID (Process ID) actuel. De plus, la fonction utilise un algorithme pour garantir que les noms de fichier seront uniques même s'il y a plusieurs appels à la fonction en même temps.

Il est également important de noter que les fichiers temporaires créés par Gleam ne seront pas automatiquement supprimés à la fin de l'exécution du programme. Il est de la responsabilité du développeur de supprimer ces fichiers après leur utilisation en utilisant la fonction `File.delete`.

## Voir Aussi

Pour en savoir plus sur la création de fichiers temporaires en Gleam, n'hésitez pas à consulter ces ressources :

- [Documentation officielle de Gleam sur la gestion des fichiers](https://gleam.run/documentation/standard_library/file.html)
- [Exemple de création de fichiers temporaires en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/file/tempfile.gleam)

Merci d'avoir lu cet article et n'oubliez pas de créer et de supprimer vos fichiers temporaires de manière responsable !