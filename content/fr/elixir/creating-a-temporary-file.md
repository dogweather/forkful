---
title:                "Elixir: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi créer un fichier temporaire en Elixir ?

Créer un fichier temporaire peut être utile dans de nombreuses situations de programmation en Elixir. Cela peut être nécessaire pour stocker temporairement des données, effectuer des opérations de manipulation de fichiers ou simplement pour des tâches de test et de débogage.

# Comment créer un fichier temporaire en Elixir

Pour créer un fichier temporaire en Elixir, nous pouvons utiliser la fonction `File.tmp` fournie par le module `File`. Cette fonction prend en paramètre le préfixe du nom du fichier et le suffixe, et retourne un tuple contenant le chemin et le descripteur de fichier.

```Elixir
{file_path, file_descriptor} = File.tmp("temp_", ".txt")
```

Nous pouvons ensuite utiliser le descripteur de fichier pour écrire des données dans le fichier temporaire.

```Elixir
file_descriptor |> IO.write("Hello world")
```

Une fois que nous avons fini d'utiliser le fichier, nous pouvons le supprimer en utilisant la fonction `File.rm`.

```Elixir
File.rm(file_path)
```

# Un aperçu plus détaillé de la création d'un fichier temporaire

Lorsque nous utilisons la fonction `File.tmp`, Elixir crée un fichier dans le répertoire temporaire du système d'exploitation en utilisant le préfixe et le suffixe fournis. Cela garantit que le nom du fichier est unique et évite les conflits avec d'autres fichiers existants.

En plus du chemin et du descripteur de fichier, la fonction `File.tmp` renvoie également une liste de fichiers temporaires créés. Cela peut être utile si nous devons créer plusieurs fichiers temporaires dans une même opération.

# Voir aussi

- [Documentation Elixir sur la création de fichiers temporaires](https://hexdocs.pm/elixir/File.html#tmp/2)
- [Tutoriel sur la manipulation de fichiers en Elixir](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
- [Article sur les avantages de l'utilisation de fichiers temporaires](https://www.perforce.com/blog/vcs/4-reasons-track-temporary-files-perforce)