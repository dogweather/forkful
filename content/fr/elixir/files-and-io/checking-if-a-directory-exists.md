---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:58.501537-07:00
description: "Comment faire : La biblioth\xE8que standard d'Elixir offre une mani\xE8\
  re directe de v\xE9rifier l'existence d'un r\xE9pertoire \xE0 travers le module\
  \ `File`. Voici\u2026"
lastmod: '2024-03-13T22:44:57.342908-06:00'
model: gpt-4-0125-preview
summary: "La biblioth\xE8que standard d'Elixir offre une mani\xE8re directe de v\xE9\
  rifier l'existence d'un r\xE9pertoire \xE0 travers le module `File`."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

## Comment faire :
La bibliothèque standard d'Elixir offre une manière directe de vérifier l'existence d'un répertoire à travers le module `File`. Voici comment vous pouvez l'utiliser :

```elixir
if File.dir?("chemin/vers/repertoire") do
  IO.puts "Le répertoire existe !"
else
  IO.puts "Le répertoire n'existe pas."
end
```

Exemple de sortie, en supposant que le répertoire n'existe pas :
```
Le répertoire n'existe pas.
```

Pour des interactions plus avancées avec le système de fichiers, incluant la vérification de l'existence de répertoires, vous pourriez envisager d'utiliser des bibliothèques tierces comme `FileSystem`. Bien que les capacités standard d'Elixir soient suffisantes pour de nombreux cas, `FileSystem` peut offrir un contrôle et des retours plus nuancés pour des applications complexes. Cependant, pour le besoin basique de vérifier si un répertoire existe, il est généralement recommandé de s'en tenir au module natif `File`, car il est facilement disponible et ne nécessite aucune dépendance externe.
