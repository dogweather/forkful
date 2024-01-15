---
title:                "Création d'un fichier temporaire"
html_title:           "Fish Shell: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire?

Il y a plusieurs raisons pour lesquelles vous pourriez avoir besoin de créer un fichier temporaire dans votre code Fish Shell. Un des exemples les plus courants est de stocker temporairement des données avant de les enregistrer dans un fichier permanent. Cela peut être utile lors de la manipulation de gros volumes de données ou lors de la mise en œuvre de fonctionnalités telles que l'annulation ou la restauration.

## Comment faire?

Il existe plusieurs façons de créer un fichier temporaire dans le code Fish Shell. Voici deux exemples très simples pour vous aider à démarrer:

```Fish Shell
# Avec la commande built-in "mktemp"
set temp_file (mktemp)

# Avec l'utilitaire "touch"
touch $HOME/temp_file.txt
```

Dans le premier exemple, nous utilisons la commande *built-in* "mktemp" qui crée un fichier temporaire avec un nom unique et stocke son chemin d'accès dans la variable "temp_file". Dans le deuxième exemple, nous utilisons l'utilitaire "touch" pour créer un fichier vide dans le répertoire "HOME". Pour accéder au contenu de ce fichier, nous pouvons utiliser la commande "cat" ou toute autre commande de lecture de fichier.

## Plongée en profondeur

Créer un fichier temporaire n'est pas seulement utile pour stocker des données temporaires, cela peut également aider à améliorer les performances de votre code. En créant un fichier temporaire, vous pouvez économiser de précieuses ressources telles que la mémoire vive en limitant la quantité de données stockées en mémoire.

Il est également important de choisir un nom de fichier unique pour éviter d'éventuels conflits avec d'autres fichiers dans votre système. Ce qui rend la commande "mktemp" si pratique, c'est qu'elle génère automatiquement un nom de fichier unique pour vous.

## Voir aussi

- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Un tutoriel sur la manipulation de fichiers dans Fish Shell](https://linuxhint.com/tmp_file_handling_fish_shell/)
- [Plus d'astuces et de conseils pour optimiser votre code Fish Shell](https://linuxhint.com/fish_shell_tips_tricks/)