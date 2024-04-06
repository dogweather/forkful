---
date: 2024-01-20 17:53:53.809833-07:00
description: 'Comment faire : Output.'
lastmod: '2024-04-05T21:53:58.924810-06:00'
model: gpt-4-1106-preview
summary: ''
title: Lecture d'un fichier texte
weight: 22
---

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
