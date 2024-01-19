---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Lire un fichier texte, c'est simplement récupérer et utiliser les données stockées dans ce type de fichier à l'aide d'un programme. Les programmeurs le font pour manipuler et analyser les informations contenues dans ces fichiers, pour la recherche de motifs, le traitement de données, et plus encore.

## Comment faire:

Voici comment lire un fichier texte en Ruby.

```Ruby
# Ouverture et lecture du fichier
fichier = File.open('chemin_vers_votre_fichier.txt', 'r')
while (ligne = fichier.gets)
   puts ligne
end
fichier.close
```

Sortie d'échantillon:

```Ruby
Ceci est la ligne 1 du fichier
Ceci est la ligne 2 du fichier
Ceci est la ligne 3 du fichier
```

## Plongée en profondeur:

Les fichiers textes ont été parmi les premiers types de données à être manipulés par des programmes informatiques. Ruby, introduit en 1995, a toujours facilité la lecture des fichiers texte.

Comme alternative, vous pouvez utiliser `File.readlines('chemin_vers_votre_fichier.txt')` pour lire toutes les lignes à la fois.

Les détails d'implémentation comprennent la gestion des erreurs. Par exemple, que se passe-t-il si le fichier n'est pas trouvé? Ou si vous n'avez pas la permission de le lire? Ruby propose des mécanismes pour gérer ces exceptions.

## Voir aussi:

- Documentation officielle de Ruby sur la Classe File : https://ruby-doc.org/core/File.html
- Un tutoriel sur la manipulation de fichiers texte en Ruby : http://zetcode.com/lang/rubytutorial/io/
- Gestion des exceptions en Ruby : https://www.tutorialspoint.com/ruby/ruby_exceptions.htm