---
title:                "Écrire un fichier texte"
html_title:           "Elixir: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Qu'est-ce que & Pourquoi?

Ecrire un fichier texte, c'est simplement enregistrer des informations sous forme de texte dans un fichier sur votre ordinateur. Les programmeurs le font principalement pour stocker et rendre l'information accessible pour leur application.

# Comment faire :

```
Elixir

File.write("mon_fichier.txt", "Voici mon texte à enregistrer!")

```

Ce code enregistre le texte "Voici mon texte à enregistrer!" dans un fichier appelé "mon_fichier.txt". Vous pouvez ensuite lire ce fichier avec le code suivant :

```
Elixir

File.read("mon_fichier.txt")

```

Ce qui vous donnera l'output suivant :

```
{:ok, "Voici mon texte à enregistrer!"}
```

# Plongée en profondeur :

Historiquement, les fichiers texte étaient la seule option pour stocker des données. Maintenant, il existe d'autres options telles que les bases de données, mais les fichiers texte restent utiles pour de nombreuses tâches informatiques simples. Dans Elixir, vous pouvez utiliser la librairie stdlib pour travailler avec des fichiers texte en utilisant les modules ```File``` et ```IO```. Vous pouvez également automatiser l'écriture de fichiers texte en utilisant des templates avec des bibliothèques comme EEx.

# Voir aussi :

- Documentation sur la librairie stdlib pour travailler avec des fichiers : https://hexdocs.pm/elixir/File.html
- Tutoriel sur l'utilisation de fichiers texte en Elixir : https://elixirschool.com/fr/lessons/libraries/file/
- Tutoriel sur EEx pour automatiser l'écriture de fichiers texte: https://elixirschool.com/fr/lessons/basics/templating-with-eex/