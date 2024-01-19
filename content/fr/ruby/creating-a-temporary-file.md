---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi et Pour Quoi?

La création d'un fichier temporaire est une technique qui permet de stocker des données de manière provisoire pendant l'exécution d'un programme. Les développeurs l'utilisent fréquemment pour manipuler d'énormes quantités de données sans consommer de l'espace mémoire excessif et pour préserver les données entre les sessions d'exécution d'un programme.


## Comment faire :

Créer un fichier temporaire avec Ruby est simple grâce à la bibliothèque intégrée `Tempfile`. Regardez cet exemple :

```Ruby
require 'tempfile'

# Création d'un fichier temporaire
mon_fichier_temp = Tempfile.new('mon_fichier')

# Écriture dans le fichier temporaire
mon_fichier_temp.write("Salut Ruby !")

# Lecture du contenu du fichier temporaire
mon_fichier_temp.seek(0)

puts mon_fichier_temp.read
```

Si vous exécutez ce code, vous verrez :

```Shell
Salut Ruby !
```

## Quelques Détails :

Historiquement, la création de fichiers temporaires était un moyen courant de gérer l'échange de données entre différents programmes avant l'apparition des bases de données. 

Comme alternatives, on peut citer les fichiers réguliers ou l'utilisation de la mémoire vive (RAM). Cependant, garder à l'esprit les restrictions : les fichiers réguliers encombrent votre système à long terme, et la RAM a une capacité limitée et ne préserve pas les données après le redémarrage du programme.

Quant à la mise en œuvre, les fichiers temporaires sont placés par défaut dans le répertoire temporaire du système (dépend du système d'exploitation : `/tmp` sur Unix, `%TEMP%` sur Windows).

## Voir Aussi :

- Documentation officielle de Ruby sur `Tempfile` : [https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html](https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html)
- Article StackOverflow sur le sujet : [https://stackoverflow.com/questions/357754/how-can-i-create-a-temporary-file-with-ruby](https://stackoverflow.com/questions/357754/how-can-i-create-a-temporary-file-with-ruby) 
- Détails d'implémentation des fichiers temporaires sur différent OS : [https://en.wikipedia.org/wiki/Temporary_folder](https://en.wikipedia.org/wiki/Temporary_folder)