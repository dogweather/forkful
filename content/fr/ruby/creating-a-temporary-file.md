---
title:                "Ruby: Création d'un fichier temporaire"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en Ruby ?

La création d'un fichier temporaire peut être utile dans de nombreuses situations en programmation Ruby. Par exemple, si vous avez besoin de stocker temporairement des données avant de les traiter, ou si vous travaillez avec des fichiers volumineux et que vous voulez éviter de les modifier directement, créer un fichier temporaire peut être une bonne solution.

## Comment créer un fichier temporaire en Ruby ?

Voici un exemple de code qui vous montre comment créer et écrire dans un fichier temporaire en utilisant Ruby :

```ruby
#!/usr/bin/env ruby

require "tempfile"

# Créer un fichier temporaire
temp_file = Tempfile.new("mon_fichier_temporaire.txt")

# Ecrire dans le fichier temporaire
temp_file.puts "Bonjour le monde !"

# Fermer et supprimer le fichier temporaire
temp_file.close
temp_file.unlink
```

Si vous exécutez ce code, un fichier temporaire nommé "mon_fichier_temporaire.txt" sera créé dans le répertoire où le script est exécuté, et la phrase "Bonjour le monde !" sera écrite dedans.

## Plongée profonde dans la création d'un fichier temporaire en Ruby

Lorsque vous utilisez la méthode `Tempfile.new`, Ruby crée automatiquement un fichier avec un nom unique et le place dans le répertoire temporaire de votre système d'exploitation. Vous pouvez également spécifier un préfixe pour le nom du fichier temporaire en passant un argument au format string (par exemple `Tempfile.new("mon_fichier_")`).

De plus, si vous voulez donner un suffixe spécifique au fichier temporaire (par exemple `.txt`), vous pouvez utiliser la méthode `Tempfile.create` au lieu de `Tempfile.new`.

Il est également important de noter que les fichiers temporaires créés avec Ruby sont automatiquement supprimés lorsque le programme se termine ou lorsque vous appelez la méthode `unlink` sur le fichier temporaire.

## Voir aussi

- [Documentation officielle de la classe Tempfile en Ruby](https://ruby-doc.org/stdlib/libdoc/tempfile/rdoc/Tempfile.html)
- [Tutoriel sur la manipulation des fichiers en Ruby](https://www.rubyguides.com/ruby-tutorial/working-with-files/)
- [Exemples de code pour créer des fichiers temporaires en Ruby](https://www.dotnetperls.com/tempfile-ruby)