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

## Quoi & Pourquoi?
Lecture Fichier Texte est une opération courante pour les programmeurs d'Elixir, qui leur permet de lire et d'interpréter le contenu d'un fichier texte. Cela est particulièrement utile lorsque l'on souhaite extraire des données d'un fichier ou traiter un texte de manière automatique.

## Comment faire:
Voici comment lire un fichier texte en utilisant Elixir:
```Elixir
File.read("fichier.txt")
```
Le résultat sera une chaîne de caractères contenant le contenu du fichier. Si l'on souhaite spécifier un encodage, on peut le faire en ajoutant une option à la fonction `read`:
```Elixir
File.read("fichier.txt", encoding: :utf8)
```

## Approfondissement:
Les fichiers textes sont une partie essentielle de l'informatique depuis les débuts de la programmation. Ils ont été utilisés pour stocker des données et échanger des informations entre différentes applications. Aujourd'hui, il existe d'autres formats de fichiers tels que JSON ou XML, mais les fichiers textes restent importants pour des tâches simples et rapides.

Pour lire un fichier ligne par ligne, on peut utiliser la fonction `Stream` :
```Elixir
File.stream!("fichier.txt") |> Enum.each(&IO.puts/1)
```

## Voir aussi:
- Documentation sur la fonction [`File.read/2`](https://hexdocs.pm/elixir/File.html#read/2) 
- Article sur la manipulation de fichiers en Elixir : [Manipuler des fichiers en Elixir](https://blog.lelonek.me/working-with-files-in-elixir-part-1-9fdf751eef34)
- Tutoriel vidéo pour lire un fichier texte en utilisant Elixir : [Lire un fichier texte en Elixir](https://www.youtube.com/watch?v=ORzX5NJtm1E)