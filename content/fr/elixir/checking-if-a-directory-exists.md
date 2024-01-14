---
title:    "Elixir: Vérifier si un répertoire existe"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de plonger dans comment vérifier si un répertoire existe en Elixir, il est important de comprendre pourquoi cette tâche peut être utile. En tant que développeur, il est courant de devoir manipuler des fichiers et des répertoires dans nos applications. Savoir si un répertoire existe ou non peut nous aider à planifier nos actions en conséquence et à éviter des erreurs imprévues.

## Comment faire

Pour vérifier si un répertoire existe en Elixir, nous pouvons utiliser la fonction `File.dir?/1` fournie par le module `File`. Voici un exemple de code qui vérifie si un répertoire nommé "documents" existe dans le répertoire courant :

```Elixir
if File.dir?("documents") do
  IO.puts "Le répertoire existe."
else
  IO.puts "Le répertoire n'existe pas."
end
```

Si le répertoire "documents" existe, la fonction `File.dir?/1` renverra `true` et nous afficherons le message "Le répertoire existe.". Si le répertoire n'existe pas, elle renverra `false` et nous afficherons le message "Le répertoire n'existe pas.".

## Plongée en profondeur

La fonction `File.dir?/1` utilise la fonction sous-jacente `:file.read_dir_info/1`, qui renvoie des informations sur un répertoire. Si le répertoire n'existe pas, `:file.read_dir_info/1` renverra une erreur. Donc, en utilisant `File.dir?/1`, nous évitons de devoir gérer des erreurs inutiles et nous pouvons simplement nous concentrer sur si un répertoire existe ou non.

## Voir aussi

- [Documentation officielle d'Elixir sur la gestion des fichiers](https://hexdocs.pm/elixir/File.html)
- [Example pratique de la fonction File.dir?/1](https://elixirschool.com/fr/lessons/basics/basics-dir/)
- [Tutoriel vidéo sur la manipulation des répertoires en Elixir](https://www.youtube.com/watch?v=2sZQGT4KqLg)