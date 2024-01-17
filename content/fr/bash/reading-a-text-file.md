---
title:                "Lecture d'un fichier texte"
html_title:           "Bash: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Lire un fichier texte (text file) signifie simplement ouvrir un fichier qui contient du texte et en afficher le contenu. Les programmeurs le font souvent pour accéder à des données stockées dans un format lisible par l'homme et les utiliser dans leur code.

## Comment faire:

Voici un exemple de code Bash pour lire un fichier texte et afficher chaque ligne:

```Bash
#!/bin/bash

# Définit la variable FICHIER avec le chemin du fichier à lire
FICHIER="/chemin/fichier.txt"

# Utilise la commande "cat" pour afficher le contenu du fichier
cat $FICHIER
```

Exemple de résultat en utilisant un fichier contenant les lignes "Bonjour" et "Au revoir":
```Bash
Bonjour
Au revoir
```

## Plongée en profondeur:

La lecture de fichiers texte a toujours été une fonctionnalité essentielle pour les programmeurs, car elle offre une façon pratique d'échanger des données entre les différentes parties d'un programme. Avant l'avènement de l'informatique moderne, les programmeurs utilisaient des cartes perforées et d'autres formes de stockage physique pour enregistrer leurs données. Cela rendait la lecture des informations beaucoup plus compliquée et sujette aux erreurs.

De nos jours, il existe de nombreuses alternatives pour lire des fichiers texte, telles que les commandes awk, sed et grep, qui permettent de filtrer et de manipuler le contenu du fichier. Des langages comme Python offrent également des solutions efficaces pour lire, écrire et traiter des fichiers texte.

En termes d'implémentation, la lecture d'un fichier texte implique principalement d'utiliser des commandes système pour ouvrir, lire et fermer le fichier. Les programmeurs doivent également tenir compte des différentes encodages de caractères et des caractères spéciaux pouvant être présents dans le fichier.

## Voir aussi:

- [La commande "cat" sur Linuxize.com](https://linuxize.com/post/cat-command-in-linux/)
- [La différence entre lire des fichiers binaires et texte sur StackOverflow (en anglais)](https://stackoverflow.com/questions/1359810/difference-between-binary-and-text-file)
- [Documentation Bash officielle sur la commande "cat"](https://www.gnu.org/software/bash/manual/html_node/cat.html)