---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:58:18.018365-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"

category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Vérifier si un répertoire existe, c'est s'assurer qu'un dossier est bien là où on le pense. Les programmeurs font ça pour éviter les erreurs de fichier et s'adapter dynamiquement aux données.

## How to:
```Ruby
require 'fileutils'

# Verifier si un répertoire existe
if Dir.exist?('/chemin/vers/repertoire')
  puts 'Le répertoire existe!'
else
  puts 'Le répertoire n\'existe pas.'
end
```
Output possible:
```
Le répertoire existe!
```
Ou:
```
Le répertoire n'existe pas.
```

## Deep Dive
Avant, on utilisait souvent `File.directory?` pour tester la présence d'un répertoire. `Dir.exist?` et `File.directory?` sont généralement interchangeables, mais `Dir.exist?` communique plus clairement l’intention de vérifier un répertoire — un peu comme on choisirait une meilleure légende pour une photo.

On peut aussi trouver `FileUtils.mkdir_p`, qui crée un répertoire s'il n'existe pas déjà, utile pour préparer l'espace de travail sans faire d'erreurs.

En coulisse, ces méthodes vont interroger le système de fichiers, ce qui peut être plus ou moins coûteux en fonction du système d'exploitation et du type de stockage. C'est instantané sur un SSD local, mais peut prendre plus de temps sur un réseau ou un disque dur externe.

## See Also
- [Documentation Ruby pour Dir.exist?](https://ruby-doc.org/core-3.1.0/Dir.html#method-c-exist-3F)
- [Documentation Ruby pour FileUtils](https://ruby-doc.org/stdlib-3.1.0/libdoc/fileutils/rdoc/FileUtils.html)
- [Stack Overflow: Comment vérifier si un répertoire existe](https://stackoverflow.com/questions/5471032/how-do-i-check-if-a-directory-exists-in-ruby)
