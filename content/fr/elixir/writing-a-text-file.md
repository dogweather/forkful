---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire dans un fichier texte permet de sauvegarder des données pour les réutiliser plus tard. Les programmeurs le font pour des logs, des configurations, des exportations de données, etc.

## How to:
```elixir
# Création d'un fichier texte avec Elixir

# Ouvrir ou créer un fichier en mode écriture
{:ok, file} = File.open("example.txt", [:write])

# Écriture de texte dans le fichier
IO.write(file, "Bonjour le monde d'Elixir!\n")

# Écriture de texte avec une nouvelle ligne
IO.puts(file, "C'est facile écrire dans un fichier.")

# Fermeture du fichier
File.close(file)
```

## Deep Dive
Elixir, créé en 2011 par José Valim, est conçu pour être facile à utiliser pour les IO comme l'écriture de fichiers, s'appuyant sur Erlang VM pour la robustesse. Les alternatives incluent l'utilisation de la fonction `File.write/2`, qui offre un raccourci pour écrire dans des fichiers sans les ouvrir explicitement. Pour des gros volumes de données, on utilise souvent `Stream` pour une écriture efficace et paresseuse.

## See Also
- [Documentation Elixir de IO](https://hexdocs.pm/elixir/IO.html)
- [Documentation Elixir de File](https://hexdocs.pm/elixir/File.html)
- [Guide d'apprentissage Elixir](https://elixir-lang.org/learning.html)
