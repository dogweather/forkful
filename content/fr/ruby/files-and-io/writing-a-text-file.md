---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:04.067481-07:00
description: "\xC9crire dans un fichier texte en Ruby est une op\xE9ration fondamentale\
  \ qui vous permet de stocker des sorties et des donn\xE9es de mani\xE8re persistante,\u2026"
lastmod: '2024-03-13T22:44:58.440323-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire dans un fichier texte en Ruby est une op\xE9ration fondamentale\
  \ qui vous permet de stocker des sorties et des donn\xE9es de mani\xE8re persistante,\
  \ permettant ainsi aux donn\xE9es d'\xEAtre consult\xE9es ou modifi\xE9es ult\xE9\
  rieurement."
title: "R\xE9diger un fichier texte"
weight: 24
---

## Comment faire :
Ruby rend les opérations sur les fichiers simples. Pour écrire dans un fichier, vous pouvez utiliser la classe `File` intégrée à Ruby. L'exemple suivant démontre comment ouvrir un fichier pour l'écriture (mode `"w"`) et pour l'append (mode `"a"`), puis écrire une chaîne dans celui-ci et s'assurer que le fichier est fermé ensuite :

```ruby
# Écrire un nouveau contenu dans un fichier, en écrasant le contenu existant
File.open("example.txt", "w") do |file|
  file.puts "Bonjour, Ruby!"
end

# Ajouter du contenu à la fin d'un fichier
File.open("example.txt", "a") do |file|
  file.puts "Ajout d'une autre ligne."
end
```
Après avoir exécuté les deux extraits, le contenu de `example.txt` sera :
```
Bonjour, Ruby!
Ajout d'une autre ligne.
```

### Utiliser une bibliothèque tierce : FileUtils
Pour des opérations sur les fichiers plus complexes, la bibliothèque standard de Ruby `FileUtils` peut être très utile, bien que pour l'écriture de fichiers basique, les méthodes standard de `File` soient suffisantes. Cependant, si vous souhaitez copier, déplacer, supprimer ou effectuer d'autres opérations sur le système de fichiers en conjonction avec l'écriture de fichiers, `FileUtils` vaut la peine d'être explorée.

Un exemple d'utilisation de `FileUtils` pour créer un répertoire puis écrire dans un fichier dans ce répertoire :
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/aujourd'hui.log", "w") do |file|
  file.puts "Entrée du journal : #{Time.now}"
end
```

Cela démontre la création d'un nouveau répertoire `logs` s'il n'existe pas déjà, et l'écriture dans un nouveau fichier `aujourd'hui.log` à l'intérieur, montrant à la fois la manipulation de répertoires et de fichiers sans écrire directement avec FileUtils, mais en utilisant sa capacité de gestion de répertoires.
