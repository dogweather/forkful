---
title:                "Elixir: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de rentrer dans le vif du sujet, il est important de comprendre pourquoi il est nécessaire de vérifier l'existence d'un répertoire en programmation en Elixir. Cette étape est essentielle pour s'assurer que le répertoire cible existe avant de le manipuler ou de tenter de le créer. Cela permet également de gérer les erreurs et exceptions qui pourraient survenir lors de l'accès au répertoire.

## Comment procéder

Vérifier l'existence d'un répertoire en Elixir est une tâche relativement simple. Tout d'abord, nous devons importer le module `File` qui contient la fonction `exists?/1` pour vérifier l'existence d'un répertoire. Ensuite, nous pouvons utiliser cette fonction en lui passant en paramètre le chemin du répertoire cible sous forme de chaîne de caractères.

```
Elixir
IO.puts File.exists?("chemin/répertoire")
```

Si le répertoire existe, la fonction renverra `true`, sinon elle renverra `false`. Nous pouvons également utiliser la fonction `is_directory?/1` pour vérifier si le chemin fourni correspond bien à un répertoire et non à un fichier.

```
Elixir
is_dir = is_directory?("chemin/répertoire")
IO.puts "Le chemin fourni correspond-il à un répertoire ? #{is_dir}"
```

## Approfondissement

La fonction `exists?/1` du module `File` utilise en interne la fonction du système d'exploitation respective. Par exemple, sur un système Linux, elle utilise la fonction `stat()` pour vérifier l'existence d'un fichier ou d'un répertoire. Il est donc important de prendre en compte les différentes spécificités du système d'exploitation sur lequel votre code sera exécuté.

## Voir aussi

- Documentation officielle sur la fonction `exists?/1` du module `File` : https://hexdocs.pm/elixir/File.html#exists?/1
- Article sur les manipulations de fichiers et répertoires en Elixir : https://elixir-lang.org/getting-started/mix-otp/file-system-and-environment.html
- One article en anglais sur la vérification de l'existence d'un répertoire en Elixir : https://www.poeticoding.com/how-to-check-if-a-directory-exists-in-elixir/