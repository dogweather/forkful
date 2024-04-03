---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:08.698753-07:00
description: "V\xE9rifier si un r\xE9pertoire existe en Ruby permet aux programmeurs\
  \ de confirmer la pr\xE9sence d'un r\xE9pertoire avant d'effectuer des op\xE9rations\
  \ comme la\u2026"
lastmod: '2024-03-13T22:44:58.435032-06:00'
model: gpt-4-0125-preview
summary: "V\xE9rifier si un r\xE9pertoire existe en Ruby permet aux programmeurs de\
  \ confirmer la pr\xE9sence d'un r\xE9pertoire avant d'effectuer des op\xE9rations\
  \ comme la lecture de fichiers ou la cr\xE9ation de nouveaux r\xE9pertoires."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

## Comment faire :
La bibliothèque standard de Ruby fournit des méthodes simples pour vérifier l'existence d'un répertoire. Voici comment le faire avec Ruby pur, sans avoir besoin de bibliothèques tierces :

```ruby
require 'fileutils'

# Vérifier si un répertoire existe
if Dir.exist?('/chemin/vers/repertoire')
  puts 'Le répertoire existe.'
else
  puts 'Le répertoire n'existe pas.'
end
```
Exemple de sortie :
```
Le répertoire existe.
```
Ou :
```
Le répertoire n'existe pas.
```

En plus d'utiliser `Dir.exist?`, vous pouvez également utiliser la méthode `File.directory?` qui retourne `true` si le chemin donné est un répertoire :

```ruby
if File.directory?('/chemin/vers/repertoire')
  puts 'Le répertoire existe.'
else
  puts 'Le répertoire n'existe pas.'
end
```
Les méthodes `Dir.exist?` et `File.directory?` font partie de la bibliothèque standard de Ruby et ne nécessitent aucun gem externe pour être utilisées, les rendant des options pratiques et efficaces pour les vérifications de répertoires.
