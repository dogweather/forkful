---
title:                "Vérifier si un répertoire existe"
date:                  2024-02-03T19:06:58.501537-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Vérifier si un répertoire existe dans Elixir consiste à confirmer la présence d'un répertoire à un chemin spécifié dans le système de fichiers. Les programmeurs font cela pour s'assurer qu'ils peuvent lire, écrire ou effectuer des opérations sur le répertoire en toute sécurité sans rencontrer d'erreurs dues à son absence.

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
