---
title:                "Écrire un fichier texte"
html_title:           "Ruby: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant ou même expérimenté, vous avez probablement déjà entendu parler du langage de programmation Ruby. L'une des raisons pour lesquelles il est si populaire est sa facilité d'utilisation pour écrire des fichiers texte. Dans cet article, nous allons vous montrer comment écrire un fichier texte en utilisant Ruby et vous donner un aperçu plus approfondi de ce processus.

## Comment faire

Pour écrire un fichier texte en Ruby, vous devez tout d'abord ouvrir un nouveau fichier en utilisant la méthode `File.new()` en spécifiant le nom du fichier et le mode d'accès. Vous pouvez utiliser `w` pour écrire ou écraser le contenu existant du fichier, `a` pour ajouter du contenu à la fin du fichier ou `r+` pour lire et écrire dans le fichier.

```
# Créer un nouveau fichier
file = File.new("mon_fichier.txt", "w")

# Ajouter du contenu au fichier
file.write("Bonjour à tous !")
file.close # N'oubliez pas de fermer le fichier une fois que vous avez fini d'écrire.
```

Vous pouvez également utiliser `File.open()` qui ouvre automatiquement et ferme le fichier une fois que vous avez fini de l'utiliser.

```
# Ouvrir un fichier en mode ajout
file = File.open("mon_fichier.txt", "a")

# Ajouter du contenu à la fin du fichier
file.puts("Voici une nouvelle ligne ajoutée !")
file.close
```

## Plongée en profondeur

En plus des méthodes `File.new()` et `File.open()`, Ruby offre également des méthodes pour lire et manipuler des fichiers texte. Par exemple, `File.read()` lit tout le contenu d'un fichier et le renvoie sous forme d'une seule chaîne de caractères. Il existe également des options pour lire le contenu ligne par ligne ou pour ignorer les blancs.

```
# Lire tout le contenu d'un fichier
file = File.read("mon_fichier.txt")
puts file # Output: "Bonjour à tous ! Voici une nouvelle ligne ajoutée !"

# Lire le contenu ligne par ligne
file = File.open("mon_fichier.txt")
file.each do |line|
  puts line # Output: "Bonjour à tous !" et "Voici une nouvelle ligne ajoutée !"
end
file.close
```

Vous pouvez également utiliser la méthode `File.exist?()` pour vérifier si un fichier existe avant de l'ouvrir ou l'écrire, et la méthode `File.delete()` pour supprimer un fichier.

## Voir aussi

- [Documentation sur les fichiers en Ruby](https://ruby-doc.org/core-3.0.2/File.html)
- [Tutoriel sur les fichiers en Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Exemples pratiques d'utilisation de fichiers en Ruby](https://www.ruby-forum.com/t/15-handy-ruby-file-examples/)