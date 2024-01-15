---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Elixir: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elixir, il est probable que vous ayez besoin de vérifier si un répertoire existe avant d'exécuter une opération dessus. Cela peut être utile pour éviter les erreurs, gérer les chemins d'accès et assurer la robustesse de votre code.

## Comment Faire

Pour vérifier si un répertoire existe en utilisant Elixir, vous pouvez utiliser la fonction `File.dir?/1` de la bibliothèque standard. Elle prend en paramètre le chemin absolu ou relatif du répertoire à vérifier et renvoie `true` si le répertoire existe et `false` sinon.

```Elixir
iex> File.dir?("/chemin/vers/mon/repertoire")
true

iex> File.dir?("inexistant")
false
```

Vous pouvez également utiliser `Path.join/2` pour construire le chemin d'accès avant de le passer à `File.dir?/1`. Voici un exemple:

```Elixir
iex> repertoire = "mon" |> Path.join("chemin") |> Path.join("vers") |> Path.join("repertoire")
iex> File.dir?(repertoire)
true
```

## Plongée Profonde

La fonction `File.dir?/1` utilise la fonction sous-jacente `:file.dir?` de la bibliothèque C Erlang pour vérifier si un répertoire existe. En cas d'échec, la fonction renverra l'erreur `:enoent`. Vous pouvez également utiliser `File.dir?/1` sur un chemin d'accès vers un fichier, auquel cas la fonction renverra `false` même si le fichier existe.

Il est important de noter que la fonction `File.dir?/1` ne vérifie pas si le répertoire peut être accédé ou modifié. Elle se contente de vérifier si le répertoire existe. De plus, cette fonction ne fonctionnera pas correctement si elle est utilisée sur un répertoire distant (par exemple, sur un serveur FTP).

## Voir Aussi

- [La documentation de la bibliothèque standard Elixir pour `File.dir?/1`](https://hexdocs.pm/elixir/File.html#dir?/1)
- [La documentation de la bibliothèque standard Elixir pour `Path.join/2`](https://hexdocs.pm/elixir/Path.html#join/2)
- [La documentation Erlang pour `:file.dir?`](http://erlang.org/doc/man/file.html#dir?-1)