---
aliases:
- /fr/ruby/creating-a-temporary-file/
date: 2024-01-20 17:41:11.313304-07:00
description: "En programmation, cr\xE9er un fichier temporaire, c'est un peu comme\
  \ prendre des notes sur un bout de papier effa\xE7able. On fait \xE7a pour stocker\
  \ des donn\xE9es\u2026"
lastmod: 2024-02-18 23:09:09.428464
model: gpt-4-1106-preview
summary: "En programmation, cr\xE9er un fichier temporaire, c'est un peu comme prendre\
  \ des notes sur un bout de papier effa\xE7able. On fait \xE7a pour stocker des donn\xE9\
  es\u2026"
title: "Cr\xE9ation d'un fichier temporaire"
---

{{< edit_this_page >}}

## What & Why?
En programmation, créer un fichier temporaire, c'est un peu comme prendre des notes sur un bout de papier effaçable. On fait ça pour stocker des données qui nous intéressent seulement de manière éphémère, souvent pendant l'exécution d'un programme, sans encombrer le système de fichiers permanent.

## How to:
Ruby rend la création de fichiers temporaires super facile grâce à la librairie standard `Tempfile`. Voici comment on s'y prend :

```Ruby
require 'tempfile'

# Création d'un fichier temporaire
Tempfile.create('mon_temp') do |fichier|
  fichier.write('Salut le monde temporaire!')
  fichier.rewind

  # Lire le contenu du fichier
  puts fichier.read  # => Salut le monde temporaire!
end

# Après le block, le fichier est automatiquement supprimé.
```
On écrit dedans, on lit, et pouf, il disparaît quand on en a plus besoin.

## Deep Dive
Les fichiers temporaires sont là depuis les débuts des systèmes d'exploitation pour éviter de polluer les disques avec des données inutiles sur le long terme. Ruby utilise la librairie native `Tempfile` pour simplifier cette tâche.

Quelques points à savoir :
1. Tempfile est basé sur la classe `File`, donc tout ce qu'on peut faire avec `File`, on peut le faire avec `Tempfile`.
2. C'est thread-safe — plusieurs fils d'exécution peuvent utiliser leurs propres fichiers temporaires sans se marcher dessus.
3. Il existe d'autres manières de créer des fichiers temporaires en Ruby, comme avec `Dir.mktmpdir` si on veut un répertoire temporaire.

Détail de l'implémentation : Tempfile crée le fichier temporaire dans le répertoire retourné par `Dir.tmpdir`. Sur les systèmes unix, c'est généralement `/tmp`.

## See Also
Pour aller plus loin, consultez :
- Le module [Dir](https://ruby-doc.org/core/Dir.html) pour plus d'opérations sur les répertoires.
- La [doc sur la classe File](https://ruby-doc.org/core/File.html) pour tout savoir sur la manipulation des fichiers en Ruby.
