---
title:                "Bash: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Bash, vous avez probablement entendu parler de la création de fichiers temporaires dans vos scripts. Mais pourquoi dépenser du temps et de l'énergie à créer ces fichiers temporaires ? La réponse est simple : les fichiers temporaires sont utiles pour stocker des données temporaires qui ne sont pas nécessaires après l'exécution d'un programme.

## Comment faire

Il existe différentes façons de créer un fichier temporaire en utilisant Bash. Voici deux exemples de code qui vous permettront de créer un fichier temporaire et d'y stocker des données :

```
#!/bin/bash

# Créer un fichier temporaire en utilisant la commande mktemp
tempfile=$(mktemp)

# Écrire du texte dans le fichier temporaire
echo "Ceci est un exemple de texte" >> $tempfile

# Afficher le contenu du fichier temporaire
cat $tempfile

# Supprimer le fichier temporaire à la fin du script
rm $tempfile

```

```
#!/bin/bash

# Créer un fichier temporaire en utilisant la commande touch
tempfile=$(mktemp)

# Écrire du texte dans le fichier temporaire en utilisant des redirections
echo "Ceci est un autre exemple de texte" > $tempfile

# Afficher le contenu du fichier temporaire en utilisant la commande cat
cat $tempfile

# Supprimer le fichier temporaire à la fin du script
rm $tempfile

```

L'échantillon de code ci-dessus utilise la commande `mktemp` pour créer un fichier temporaire avec un nom unique et la commande `touch` pour créer un fichier vide. Ensuite, du texte est écrit dans le fichier temporaire en utilisant un opérateur de redirection, comme `>>` ou `>`. Enfin, le contenu du fichier temporaire est affiché à l'aide de la commande `cat` et le fichier est supprimé à la fin du script.

## Plongée en profondeur

La création de fichiers temporaires peut sembler simple, mais il y a quelques choses à garder à l'esprit lorsque vous utilisez cette technique dans vos scripts Bash. Tout d'abord, il est important de noter que les fichiers temporaires ne sont pas supprimés automatiquement après l'exécution d'un programme. Vous devrez utiliser la commande `rm` pour les supprimer vous-même.

Deuxièmement, vous pouvez spécifier un modèle pour le nom de votre fichier temporaire en utilisant l'option `-t` avec la commande `mktemp`. Cela peut être utile si vous avez besoin de créer plusieurs fichiers temporaires dans un même script et que vous souhaitez les différencier facilement.

Enfin, il est important de prendre des précautions supplémentaires si vous stockez des informations sensibles dans les fichiers temporaires. Vous devez vous assurer de les supprimer correctement et de ne pas les laisser traîner sur votre système une fois qu'ils ne sont plus nécessaires.

## Voir aussi

- [Documentation de la commande mktemp (en anglais)](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Documentation de la commande touch (en anglais)](https://www.gnu.org/software/coreutils/manual/html_node/touch-invocation.html)
- [Guide sur l'utilisation de fichiers temporaires en Bash (en anglais)](https://blog.finxter.com/create-temporary-files-python/)