---
title:                "Création d'un fichier temporaire"
html_title:           "Bash: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & pourquoi?

Créer un fichier temporaire est un moyen pour les programmeurs de stocker des données de manière temporaire dans leur code. Cela peut être utile lors de la manipulation de gros volumes de données ou lors de la création de scripts qui nécessitent l'utilisation de fichiers temporaires.

## Comment faire:

Pour créer un fichier temporaire en Bash, vous pouvez utiliser la commande `mktemp`. Voici un exemple de code:

```Bash
temp_file=$(mktemp)
echo "Ce fichier est temporaire" > $temp_file
cat $temp_file
```

La sortie de ce code serait: `Ce fichier est temporaire`.

## Plongée en profondeur:

La création de fichiers temporaires existe depuis longtemps et est souvent utilisée dans la programmation pour des tâches telles que la gestion des fichiers temporaires ou la création de scripts. Cependant, il existe également des alternatives telles que l'utilisation de la commande `mkdir` ou la création de fichiers dans un répertoire temporaire défini.

Pour ce qui est de l'implémentation, la commande `mktemp` crée un fichier vide avec un nom unique et le stocke dans un répertoire temporaire par défaut. Le fichier peut ensuite être utilisé et supprimé une fois que la tâche est terminée.

## À voir également:

Pour en savoir plus sur la création de fichiers temporaires en Bash, voici quelques liens utiles:

- [Documentation officielle de la commande `mktemp`](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Un tutoriel sur la manipulation de fichiers temporaires en Bash](https://www.linode.com/docs/tools-reference/tools/create-temporary-files-and-directories-in-bash/)
- [Comparaison entre les différentes méthodes de création de fichiers temporaires](https://askubuntu.com/questions/112913/how-to-create-a-temporary-file-for-a-shell-script)