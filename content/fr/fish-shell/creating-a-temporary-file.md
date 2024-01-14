---
title:                "Fish Shell: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de fichiers temporaires est courante dans la programmation, car ils offrent une solution rapide et efficace pour stocker des données temporaires sans encombrer l'espace de stockage permanent. En utilisant Fish Shell, vous pouvez facilement créer et gérer des fichiers temporaires dans vos scripts.

## Comment faire

Pour créer un fichier temporaire en utilisant Fish Shell, vous pouvez utiliser la commande `mktemp` suivie d'une variable pour stocker le chemin du fichier temporaire :

```Fish Shell
tmp_file=(mktemp)
```

Vous pouvez ensuite écrire des données dans le fichier en utilisant `echo` et rediriger la sortie dans le fichier temporaire :

```Fish Shell
echo "Ceci est un exemple de données à écrire dans le fichier temporaire" >> $tmp_file
```

Une fois terminé, vous pouvez supprimer le fichier temporaire en utilisant `rm` :

```Fish Shell
rm $tmp_file
```

## Plongée en profondeur

Il existe différentes façons de créer et de gérer des fichiers temporaires en utilisant Fish Shell. Vous pouvez spécifier un modèle pour le nom du fichier temporaire en utilisant l'option `-p` de la commande `mktemp`. Vous pouvez également spécifier un répertoire pour créer le fichier temporaire en utilisant l'option `-d`. 

De plus, vous pouvez définir une durée de vie pour le fichier temporaire en utilisant l'option `-t`. Une fois cette durée écoulée, le fichier sera automatiquement supprimé.

## Voir aussi

- [Documentation officielle de Fish Shell sur les fichiers temporaires](https://fishshell.com/docs/current/commands.html#mktemp)
- [Un tutoriel pas à pas sur la création de fichiers temporaires en utilisant Fish Shell](https://opensource.com/article/20/1/fisher-create-temp-files)
- [Un forum de discussion sur l'utilisation de fichiers temporaires avec Fish Shell](https://stackoverflow.com/questions/1339161/how-to-create-a-temporary-file-in-fish-shell)