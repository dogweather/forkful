---
title:                "Création d'un fichier temporaire"
html_title:           "Ruby: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire est souvent nécessaire lorsque l'on travaille avec des données temporaires ou que l'on souhaite stocker des informations de manière provisoire. Cela peut faciliter la manipulation de ces données et permettre une meilleure organisation du code.

## Comment faire

La création d'un fichier temporaire en Ruby est assez simple. Il suffit d'utiliser la méthode `Tempfile.create` et de préciser un préfixe pour le nom du fichier (par exemple "temp") :

```Ruby
require 'tempfile' # Importer la bibliothèque Tempfile

# Créer un fichier temporaire avec le préfixe "temp"
file = Tempfile.create("temp")
```

Ensuite, vous pouvez écrire du contenu dans ce fichier à l'aide de la méthode `write` :

```Ruby
file.write("Contenu du fichier temporaire") # Écrire dans le fichier
```

Il est également possible de lire le contenu du fichier avec la méthode `read` ou de lancer une commande dans le fichier avec la méthode `system` :

```Ruby
file.read # Lire le contenu du fichier
file.system("ls") # Exécuter la commande "ls" dans le fichier
```

Une fois que vous avez terminé d'utiliser le fichier temporaire, n'oubliez pas de le fermer avec la méthode `close` et de le supprimer avec la méthode `unlink` :

```Ruby
file.close # Fermer le fichier
file.unlink # Supprimer le fichier
```

## Plongée en profondeur

En creusant un peu plus, vous pouvez également spécifier un dossier de destination pour le fichier temporaire, ou encore définir des options telles que le mode d'accès ou le préfixe de nom. Tout cela est possible grâce aux nombreux paramètres que la méthode `create` accepte. De plus, vous pouvez également utiliser la classe `Tempfile` pour créer des objets de type fichier temporaire, offrant ainsi plus de flexibilité dans la gestion de ces fichiers provisoires.

## Voir aussi

- La documentation officielle sur la classe Tempfile : https://ruby-doc.org/stdlib-2.7.2/libdoc/tempfile/rdoc/Tempfile.html
- Un tutoriel sur la création de fichiers temporaires en Ruby : https://www.rubyguides.com/2016/05/temporary-files-ruby/
- Un exemple d'utilisation de fichiers temporaires dans un projet Ruby : https://github.com/seattlerb/tempfile