---
title:                "Lecture d'un fichier texte"
aliases:
- /fr/elixir/reading-a-text-file.md
date:                  2024-01-20T17:53:53.809833-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Lire un fichier texte en programmation, c'est récolter le contenu d'un fichier pour l'utiliser ou le modifier. En tant que programmeurs, on fait ça pour traiter des données, configurer nos apps ou juste pour interagir avec l'utilisateur.

## Comment faire :
```elixir
# Lire tout le contenu d'un fichier
{:ok, contenu} = File.read("chemin/vers/ton/fichier.txt")
IO.puts(contenu)

# Lire ligne par ligne
File.stream!("chemin/vers/ton/fichier.txt")
|> Enum.each(&IO.puts(&1))
```
Output:
```
Première ligne de ton fichier
Deuxième ligne de ton fichier
...
```

## Exploration en profondeur
Historiquement, la lecture de fichiers est l'une des opérations de base en programmation. Elixir, avec son héritage d'Erlang, met l'accent sur la robustesse et la facilité de parallélisation. Lire un fichier avec `File.read` est simple et direct, mais pour des fichiers énormes ou un streaming en direct, on utilise `File.stream!` qui lit le fichier en flux, permettant de manipuler des données gigantesques ou en temps réel sans claquer toute ta mémoire. N'oublie pas, File.stream! retourne un Stream, donc un Enum pour parcourir.

Comme alternative, on peut aussi plonger dans :gen_server et d'autres abstractions de processus pour encore plus de contrôle, mais c'est plus complexe.

## À voir également
- [Documentation Elixir sur File.read](https://hexdocs.pm/elixir/File.html#read/1)
- [Documentation Elixir sur File.stream!](https://hexdocs.pm/elixir/File.html#stream!/3)
- [Forum Elixir pour poser des questions](https://elixirforum.com/)
