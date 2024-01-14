---
title:    "Ruby: Création d'un fichier temporaire"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire est une tâche courante dans la programmation Ruby, surtout lorsque l'on travaille avec des données temporaires ou que l'on souhaite écrire des résultats dans un fichier. Cet article vous expliquera comment créer un fichier temporaire en utilisant Ruby.

## Comment faire

Pour commencer, nous devons require la bibliothèque ```tempfile```.

```Ruby
require 'tempfile'
```

Ensuite, nous pouvons créer un nouveau fichier temporaire en utilisant la méthode ```Tempfile.new```. Cette méthode prendra en premier argument le préfixe du nom du fichier temporaire. Vous pouvez également spécifier une extension de fichier facultative en utilisant le deuxième argument de la méthode.

```Ruby
temp_file = Tempfile.new('fichier_temporaire', '.txt')
```

Une fois que le fichier temporaire est créé, nous pouvons y écrire des données en utilisant la méthode ```write``` et en passant en argument la chaîne de caractère que nous voulons écrire dans le fichier.

```Ruby
temp_file.write('Ceci est un exemple de contenu pour notre fichier temporaire.')
```

Enfin, n'oubliez pas de fermer le fichier temporaire après avoir terminé l'écriture en utilisant la méthode ```close```.

```Ruby
temp_file.close
```

## Plongée en profondeur

L'une des principales raisons pour lesquelles il est important de créer un fichier temporaire est le nettoyage. En effet, lorsque vous créez un fichier temporaire, celui-ci est automatiquement supprimé lorsque vous fermez l'objet de fichier.

De plus, la méthode ```new``` accepte également d'autres arguments pour spécifier le dossier où le fichier temporaire sera créé, et si le fichier doit être créé en mode binaire.

Enfin, en utilisant la méthode ```unlink```, vous pouvez supprimer manuellement un fichier temporaire avant qu'il ne soit automatiquement supprimé.

## Voir aussi

- La documentation officielle de ```tempfile``` en Ruby: https://ruby-doc.org/stdlib-2.7.2/libdoc/tempfile/rdoc/Tempfile.html
- Un exemple d'utilisation de ```tempfile``` en Ruby: https://www.rubyguides.com/2015/02/ruby-tempfile/
- Une discussion sur la création de fichiers temporaires en Ruby: https://stackoverflow.com/questions/28581462/how-to-create-a-temp-file-with-ruby/28581706