---
title:                "Ruby: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire peut être utile dans de nombreuses situations lors de la programmation en Ruby. Par exemple, cela peut être utile pour stocker des données temporaires ou pour réaliser des opérations intermédiaires avant de les traiter définitivement.

## Comment faire

Pour créer un fichier temporaire en Ruby, vous pouvez utiliser la méthode `Tempfile.create`. Cela va créer un fichier avec un nom aléatoire et l'ouvrir en mode écriture.

```Ruby
require 'tempfile'

# Créer un nouveau fichier temporaire
temp_file = Tempfile.create

# Ajouter du contenu au fichier
temp_file.write("Ceci est un fichier temporaire créé en Ruby.")

# Fermer le fichier
temp_file.close

# Afficher le contenu du fichier
puts File.read(temp_file.path)

# Supprimer le fichier temporaire
temp_file.unlink

# Output:
# Ceci est un fichier temporaire créé en Ruby.
```

Vous pouvez également spécifier un nom de fichier personnalisé en passant un argument à la méthode `Tempfile.create`.

```Ruby
require 'tempfile'

# Créer un nouveau fichier temporaire avec un nom personnalisé
temp_file = Tempfile.create('mon_fichier_temporaire')

# Ajouter du contenu au fichier
temp_file.write("Voici mon fichier temporaire personnalisé en Ruby.")

# Fermer le fichier
temp_file.close

# Afficher le contenu du fichier
puts File.read(temp_file.path)

# Supprimer le fichier temporaire
temp_file.unlink

# Output:
# Voici mon fichier temporaire personnalisé en Ruby.
```

## Plongée en profondeur

La méthode `Tempfile.create` prend également en charge plusieurs options facultatives telles que `encoding`, `dir` et `perm`. Vous pouvez les utiliser pour spécifier l'encodage du fichier, le répertoire dans lequel le fichier sera créé et les permissions du fichier.

De plus, la méthode `Tempfile#create` renvoie un objet `Tempfile` qui possède des méthodes pratiques telles que `open` pour ouvrir le fichier dans différents modes, `rewind` pour rembobiner le fichier et `flush` pour vider le tampon et écrire les données dans le fichier.

## Voir aussi

- Documentation sur les fichiers temporaires en Ruby : https://ruby-doc.org/stdlib-2.7.1/libdoc/tempfile/rdoc/Tempfile.html
- Tutoriel sur la création et la manipulation de fichiers en Ruby : https://www.rubyguides.com/2015/05/working-with-files-ruby/
- Exemples de projets utilisant des fichiers temporaires en Ruby : https://github.com/topics/tempfile