---
title:                "Vérifier si un répertoire existe"
date:                  2024-02-03T19:08:08.698753-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Vérifier si un répertoire existe en Ruby permet aux programmeurs de confirmer la présence d'un répertoire avant d'effectuer des opérations comme la lecture de fichiers ou la création de nouveaux répertoires. C'est crucial pour éviter les erreurs dans la gestion des fichiers et assurer la fiabilité des manipulations du système de fichiers.

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
