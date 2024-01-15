---
title:                "Lecture d'un fichier texte"
html_title:           "Elixir: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La lecture de fichiers texte est une tâche commune dans la programmation. En apprenant à le faire en utilisant Elixir, vous pouvez élargir vos compétences en programmation et être en mesure de traiter de grandes quantités de données de manière efficace.

## Comment faire

Pour lire un fichier texte en utilisant Elixir, il faut d'abord ouvrir le fichier en utilisant la fonction `File.open/2`. Cette fonction prend deux arguments, le nom du fichier et le mode de lecture. Ensuite, vous pouvez utiliser la fonction `IO.read/2` pour lire le contenu du fichier dans une chaîne de caractères.

Voici un exemple de code pour lire un fichier texte et afficher son contenu :

```Elixir
File.open("mon_fichier.txt", [:read]) 
|> IO.read() 
|> IO.puts()
```

La sortie de ce code serait le contenu du fichier `mon_fichier.txt` imprimé dans la console.

## Plongée en profondeur

En utilisant Elixir, vous pouvez également traiter les fichiers ligne par ligne en utilisant la fonction `IO.stream/2`. Cette fonction permet de créer un flux d'entrée, ce qui est utile pour traiter de gros fichiers sans avoir à les charger entièrement en mémoire.

Voici un exemple de code pour lire un fichier ligne par ligne à l'aide de `IO.stream/2` :

```Elixir
IO.stream(File.open("mon_fichier.txt", [:read]), :line) 
|> Enum.each(fn line -> IO.puts(line) end)
```

Cette méthode est plus performante pour traiter de gros fichiers, car elle n'a pas besoin de charger tout le contenu dans la mémoire.

## Voir aussi

- [Documentation officielle d'Elixir sur la lecture de fichiers](https://elixir-lang.org/getting-started/file-i-o.html)
- [Tutoriel vidéo sur la lecture de fichiers en utilisant Elixir](https://www.youtube.com/watch?v=-JSq9t6RQq0)
- [Article sur l'utilisation de la fonction `IO.stream/2` en Elixir](https://medium.com/@arthur_eky/text-processing-in-elixir-process-large-files-using-streams-in-elixir-52851b1a3922)