---
title:                "Ruby: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur, il y a de fortes chances que vous ayez déjà eu besoin de télécharger une page web pour récupérer des données ou pour effectuer un scraping. Dans cet article, nous allons explorer comment le faire en utilisant le langage de programmation Ruby.

## Comment faire

Tout d'abord, nous avons besoin d'installer la gem 'open-uri' dans notre environnement Ruby en utilisant la commande `gem install open-uri`. Ensuite, dans notre fichier Ruby, nous allons importer cette gem en utilisant la ligne de code suivante :

```Ruby
require 'open-uri'
```

Maintenant, pour télécharger une page web, nous allons utiliser la méthode `.open()` avec l'URL de la page en tant que paramètre. Voici un exemple de code qui nous permet de télécharger la page d'accueil de Google :

```Ruby
require 'open-uri'

page = open("https://www.google.com")
puts page.read
```

Le `puts page.read` permet d'afficher le contenu de la page dans le terminal. Vous pouvez également utiliser d'autres méthodes comme `.gets`, `.readchar`, `.readline` pour lire le contenu de la page de différentes manières.

## Plongée en profondeur

La méthode `.open()` renvoie un objet de type `TempFile` qui se comporte comme un fichier local temporaire contenant le contenu de la page téléchargée. Cela signifie que nous pouvons utiliser toutes les méthodes de manipulation de fichiers de Ruby pour travailler avec ce contenu.

Par exemple, si nous voulons enregistrer le contenu de la page dans un fichier texte local, nous pouvons utiliser la méthode `.write()` comme ceci :

```Ruby
require 'open-uri'

page = open("https://www.google.com")
file = File.new("page_google.txt", "w+")
file.write(page.read)
puts "La page a été enregistrée dans le fichier page_google.txt !"
```

## Voir aussi

- [Documentation de la gem open-uri](https://ruby-doc.org/stdlib-2.6.3/libdoc/open-uri/rdoc/OpenURI.html)
- [Tutoriel pour le scraping en Ruby](https://www.pluralsight.com/guides/scraping-webpage-ruby)
- [Exemple de projet de scraping en Ruby](https://github.com/joeshonk/scrape-it)