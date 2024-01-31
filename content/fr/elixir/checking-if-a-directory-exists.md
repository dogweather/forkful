---
title:                "Vérification de l'existence d'un répertoire"
date:                  2024-01-19
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
En Elixir, vérifier l'existence d'un répertoire nous renseigne si on peut y lire ou écrire des données. On fait ça pour éviter des erreurs à l'exécution et gérer le flux de notre programme.

## Comment faire:

```elixir
# Utilisation de la fonction File.dir?/1
if File.dir?("path/to/your/directory") do
  IO.puts "Le répertoire existe!"
else
  IO.puts "Le répertoire n'existe pas."
end
```

Sample output:
```
Le répertoire existe!
```
ou:
```
Le répertoire n'existe pas.
```

## Plongée en profondeur

Autrefois, en Elixir, comme dans beaucoup d’autres langages de programmation, l’interaction avec le système de fichiers se faisait via des appels système dépendants du OS. Elixir, étant construit sur Erlang, utilise des abstractions pour gérer ces interactions de manière homogène sur différents systèmes d'exploitation.

Il existe aussi `:filelib.is_dir/1` depuis Erlang qui est appelé sous le capot par `File.dir?/1`. Choisir entre `File.dir?/1` et `:filelib.is_dir/1` est une question de préférence stylistique et de cohérence avec le reste de votre code.

L'implémentation détaille que `File.dir?/1` retourne un booléen, ce qui rend la logique de votre programme plus propre à écrire et à lire. Elle gère également les chemins relatifs et absolus, grâce à quoi votre code est plus fiable et portable.

## Voir aussi

- Documentation Elixir pour `File.dir?/1`: https://hexdocs.pm/elixir/File.html#dir?/1
- Documentation Erlang pour `:filelib.is_dir/1`: http://erlang.org/doc/man/filelib.html#is_dir-1
- Tutoriels Elixir pour la gestion de fichiers et répertoires: https://elixirschool.com/en/lessons/basics/collections/
