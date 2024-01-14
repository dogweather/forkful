---
title:                "Fish Shell: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire un fichier texte peut sembler être une tâche simple et banale, mais cela peut être extrêmement utile pour les programmeurs. La lecture d'un fichier texte peut fournir des informations précieuses sur des données, des modifications apportées à un code ou pour remplir automatiquement des formulaires. Dans cet article, nous allons explorer comment lire un fichier texte en utilisant Fish Shell.

## Comment faire

La première étape pour lire un fichier texte est de créer un nouveau fichier. Pour ce faire, ouvrez votre éditeur de texte préféré et créez un nouveau fichier en enregistrant le fichier avec l'extension ".txt". Ensuite, ouvrez votre terminal et assurez-vous que Fish Shell est bien installé. Maintenant, passons à la lecture du fichier texte:

```
Fish Shell
cat monfichier.txt
```

Le code ci-dessus utilise la commande "cat" pour lire le contenu du fichier texte et l'imprimer dans le terminal. Si vous avez du contenu dans votre fichier texte, vous devriez le voir s'afficher dans le terminal. Pour lire uniquement les premières lignes d'un fichier texte, vous pouvez utiliser la commande "head", et pour lire les dernières lignes, vous pouvez utiliser la commande "tail".

Par exemple, si vous souhaitez afficher les 5 premières lignes de votre fichier texte, vous pouvez utiliser la commande suivante:

```
Fish Shell
head -n 5 monfichier.txt
```

De même, si vous souhaitez afficher les 3 dernières lignes, vous pouvez utiliser la commande suivante:

```
Fish Shell
tail -n 3 monfichier.txt
```

## Plongée en profondeur

Maintenant que vous savez comment lire un fichier texte, voyons un peu plus en détail comment cela fonctionne. Le code ci-dessus utilise la commande "cat" pour lire le contenu du fichier texte et l'imprimer dans le terminal. Mais en réalité, "cat" signifie "concaténer" et peut également être utilisé pour combiner plusieurs fichiers en un seul.

Vous pouvez également utiliser la commande "grep" pour rechercher des mots ou des phrases spécifiques dans un fichier texte. Par exemple, si vous souhaitez rechercher toutes les lignes contenant le mot "bonjour" dans votre fichier texte, vous pouvez utiliser la commande suivante:

```
Fish Shell
grep "bonjour" monfichier.txt
```

De plus, vous pouvez également utiliser des commandes de traitement de texte telles que "sed" pour modifier le contenu d'un fichier texte et "awk" pour extraire des données spécifiques.

## Voir aussi

Maintenant que vous savez comment lire un fichier texte en utilisant Fish Shell, vous pouvez découvrir d'autres fonctionnalités utiles de ce shell en consultant ces liens:

- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/)
- [Un tutoriel sur les bases de Fish Shell](https://dev.to/ndesmic/intro-to-fish-shell-1p07)
- [Un guide pour travailler avec des fichiers texte en utilisant Fish Shell](https://scriptingosx.com/2019/08/moving-to-zsh-part-3-why-you-should-care-about-zsh/)