---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Création de fichiers temporaires en Bash

## Qu'est-ce Que C'est et Pourquoi?

La création de fichiers temporaires est une manip pour sauvegarder des données à court terme. Elle est utile pour gérer des charges de travail temporaire et pour stocker de l'information qui n'est pas nécessaire à long terme.

## Comment faire:

Voici comment créer un fichier temporaire en Bash. Vous verrez que c’est super simple!

```Bash
# Créer un fichier temporaire
tempfile=$(mktemp)

# Ecrire dans le fichier temporaire
echo "Voici mon fichier temporaire!" > $tempfile

# Afficher le contenu du fichier temporaire
cat $tempfile
```

Et voici à quoi ressemblent les résultats:

```Bash
Voici mon fichier temporaire!
```

## En Dur

D'abord, la notion de fichier temporaire a démarré avec UNIX dans les années 70. Bash, un descendant d’UNIX, a hérité de cette fonctionnalité. 

Ensuite, une alternative à `mktemp` est de créer votre fichier avec `tempfile`. Cependant, notez que `tempfile` est obsolète dans les versions récentes de Bash.

Enfin, `mktemp` crée un fichier dans le répertoire `/tmp` de votre système avec un nom unique pour prévenir les conflits de nommage. Vous pouvez cependant spécifier un chemin différent si nécessaire.

## Voir Aussi

Pour de plus amples informations sur la création de fichiers temporaires, consultez ces liens :

- [Documentation GNU sur mktemp](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Guide du développeur Bash](https://tldp.org/LDP/abs/html/tempfiles.html)
- [Discussion Stackoverflow sur les fichiers temporaires](https://stackoverflow.com/questions/4632028/how-to-create-a-temporary-directory)