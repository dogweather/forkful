---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:31.829985-07:00
description: "Comment faire : Elixir rend la manipulation de fichiers simple avec\
  \ des modules int\xE9gr\xE9s. La principale mani\xE8re d'\xE9crire dans un fichier\
  \ est d'utiliser\u2026"
lastmod: '2024-03-13T22:44:57.347426-06:00'
model: gpt-4-0125-preview
summary: "Elixir rend la manipulation de fichiers simple avec des modules int\xE9\
  gr\xE9s."
title: "R\xE9diger un fichier texte"
weight: 24
---

## Comment faire :
Elixir rend la manipulation de fichiers simple avec des modules intégrés. La principale manière d'écrire dans un fichier est d'utiliser les fonctions `File.write/2` ou `File.write!/2`, où la première retourne un tuple `:ok` ou `:error` et la seconde génère une erreur en cas d'échec.

Voici un exemple simple :

```elixir
# Écrire dans un fichier, message simple
File.write("hello.txt", "Bonjour, monde !")

# Lorsque vous exécutez le code, il crée 'hello.txt' avec "Bonjour, monde !" comme contenu
```

Pour ajouter à des fichiers, vous utiliseriez `File.open/3` avec les options `[:write, :append]`, puis `IO.binwrite/2` pour ajouter le contenu :

```elixir
# Ajouter à un fichier
{:ok, file} = File.open("hello.txt", [:write, :append])
IO.binwrite(file, "\nAjoutons une autre ligne.")
File.close(file)

# Maintenant, 'hello.txt' inclut une seconde ligne "Ajoutons une autre ligne."
```

Si vous travaillez avec des données volumineuses ou avez besoin de plus de contrôle sur le processus d'écriture, vous pourriez utiliser le module `Stream` pour écrire paresseusement des données dans le fichier :

```elixir
# Écrire un grand jeu de données paresseusement
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Nombre : #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn file ->
  Enum.each(stream_data, fn line ->
    IO.write(file, line)
  end)
end)

# Cela crée 'numbers.txt', écrivant les nombres de 0 à 9, chacun sur une nouvelle ligne.
```

Pour les projets nécessitant une gestion de fichiers plus sophistiquée, vous pourriez examiner des bibliothèques tierces comme `CSV`, qui offre des fonctionnalités sur mesure pour la manipulation de fichiers CSV mais souvenez-vous, pour de nombreux objectifs, les capacités intégrées d'Elixir sont plus que suffisantes.
