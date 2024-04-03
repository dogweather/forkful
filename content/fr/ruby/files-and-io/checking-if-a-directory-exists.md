---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:08.698753-07:00
description: "Comment faire : La biblioth\xE8que standard de Ruby fournit des m\xE9\
  thodes simples pour v\xE9rifier l'existence d'un r\xE9pertoire. Voici comment le\
  \ faire avec Ruby\u2026"
lastmod: '2024-03-13T22:44:58.435032-06:00'
model: gpt-4-0125-preview
summary: "La biblioth\xE8que standard de Ruby fournit des m\xE9thodes simples pour\
  \ v\xE9rifier l'existence d'un r\xE9pertoire."
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
