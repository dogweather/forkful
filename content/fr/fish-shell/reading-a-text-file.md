---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

La lecture d'un fichier texte est l'opération de récupération et d'interprétation des informations contenues dans un fichier au format texte. Les programmeurs le font pour gérer et manipuler des données, travailler avec des fichiers de configuration, lire des journaux, et mener d'autres tâches nécessitant une interaction avec des fichiers.

## Comment faire :

Voyons comment lire un fichier texte en Fish Shell. 

```Fish Shell
function lire_fichier
    for ligne in (cat $argv[1])
        echo $ligne
    end
end

lire_fichier "nom_du_fichier.txt"
```

Dans ce script, nous avons une fonction qui utilise la commande `cat` pour lire le contenu d'un fichier passé en argument. Le contenu est ensuite affiché ligne par ligne.

## Plongée en profondeur :

Historiquement, `cat` est une des plus anciennes commandes de UNIX pour la lecture des fichiers textes. Alternativement, `less` et `more` sont utilisés pour une lecture facilitée de grands fichiers.

La plupart des shells UNIX (y compris Fish) offrent la possibilité de rediriger les sorties (`>`). Vous pouvez ainsi sauvegarder le contenu de votre fichier dans une variable :

```Fish Shell
set contenu (cat "nom_du_fichier.txt")
```
Avec cette technique, il est possible d'accéder aux lignes spécifiques en utilisant leur index, par exemple `$contenu[1]` pour la première ligne.

## Voir Aussi :

Si vous souhaitez vous aventurez davantage avec les fichiers en Fish Shell, je vous recommande ces ressources :
- [Fish Shell Official Documentation](https://fishshell.com/docs/current/index.html)
- [Tutorial on reading and writing files with Fish shell](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-from-shell-scripts)
- [An Introduction to UNIX/Linux file handling](https://swcarpentry.github.io/shell-novice/04-pipefilter/index.html)