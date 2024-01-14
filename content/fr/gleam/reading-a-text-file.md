---
title:                "Gleam: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur passionné, vous cherchez sûrement à améliorer vos compétences et à apprendre de nouveaux langages de programmation. L'un de ces langages est Gleam, qui gagne en popularité grâce à sa syntaxe élégante et fonctionnelle. Dans cet article, nous allons parler de la lecture de fichiers texte en utilisant Gleam. Cela peut sembler un sujet simple, mais cela peut être une compétence importante pour de nombreux projets.

## Comment faire

Tout d'abord, nous devons importer le module [`gleam/fs`](https://gleam.run/modules/gleam_fs.html) pour pouvoir utiliser les fonctions de lecture de fichiers :

```Gleam
import gleam/fs
```

Ensuite, nous pouvons utiliser la fonction [`fs.read_file`](https://gleam.run/modules/gleam_fs.html#read_file) pour lire le contenu d'un fichier texte. Cette fonction prend en paramètre le chemin du fichier que nous voulons lire et renvoie une [`gleam/Result`](https://gleam.run/core/Result.html) contenant soit une chaîne de caractères, soit une erreur. Voici un exemple de lecture d'un fichier `texte.txt` :

```Gleam
fs.read_file("texte.txt")
```

Si le fichier est présent et lisible, le résultat sera :

```Gleam
Ok("Bonjour le monde!")
```

Si le fichier n'existe pas ou si nous n'avons pas la permission de le lire, le résultat sera :

```Gleam
Err(FileNotFound)
```

Nous pouvons également spécifier l'encodage du fichier en ajoutant un deuxième paramètre à la fonction `read_file`, par exemple :

```Gleam
fs.read_file("texte.txt", "utf-8")
```

## Deep Dive

La fonction [`fs.read_file`](https://gleam.run/modules/gleam_fs.html#read_file) utilise la fonction [`fs.slurp`](https://ulf.wtf/gleam_html/gleam.html#slurp) pour lire tout le contenu d'un fichier à la fois. Cela signifie que si le fichier est très volumineux, cela peut causer des problèmes de performance ou de mémoire. Dans ce cas, il peut être plus efficace d'utiliser la fonction [`fs.read_stream`](https://gleam.run/modules/gleam_fs.html#read_stream) qui renvoie un [stream](https://ulf.wtf/gleam_html/gleam_core/stream.html) que nous pouvons traiter ligne par ligne ou chunk par chunk.

## Voir aussi

- [Documentation officielle de Gleam sur la lecture de fichiers](https://gleam.run/modules/gleam_fs.html#read_file)
- [Guide pour débuter avec Gleam](https://gleam.run/articles/getting_started.html)
- [Projet Gleam sur GitHub](https://github.com/gleam-lang/gleam)