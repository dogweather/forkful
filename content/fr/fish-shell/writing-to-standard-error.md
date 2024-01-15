---
title:                "Écrire vers l'erreur standard"
html_title:           "Fish Shell: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur utilisant le langage Fish Shell, vous avez probablement croisé le terme "standard error" lors de votre travail sur des projets. Bien que cela puisse sembler intimidant, savoir comment écrire vers cette sortie est un outil précieux à ajouter à votre boîte à outils de développement. Dans cet article, nous allons plonger dans les raisons pour lesquelles vous devriez écrire vers le standard error et comment le faire dans Fish Shell.

## Comment faire

Pour écrire vers le standard error en utilisant Fish Shell, vous pouvez utiliser la commande `echo` suivie de `&2` pour indiquer que vous souhaitez écrire vers cette sortie. Par exemple :

```
Fish Shell : echo "Erreur !" &2
```

Ceci affichera "Erreur !" dans le standard error.

Bien sûr, il est également possible de rediriger les erreurs sorties par une commande directement vers le standard error en ajoutant `2&>` avant le nom du fichier. Par exemple :

```
Fish Shell : les commandes > fichier.txt 2&> fichier-erreurs.txt
```

Cela enverra toutes les erreurs sorties par les commandes vers le fichier "fichier-erreurs.txt".

## Deep Dive

Il peut sembler étrange de vouloir écrire vers une sortie dédiée aux erreurs, mais cela peut en fait s'avérer très utile. Lorsqu'une application ou un script génère des erreurs, celles-ci sont généralement envoyées vers la sortie standard, ce qui signifie que les utilisateurs peuvent les manquer ou même les ignorer. En écrivant vers le standard error, vous vous assurez que les erreurs sont bien visibles par l'utilisateur, ce qui peut l'aider à déboguer plus facilement.

De plus, écrire vers le standard error peut également être utile lors de l'utilisation de pipes ou de redirections dans vos scripts. En écrivant vers le standard error, vous pouvez vous assurer que les erreurs ne seront pas redirigées vers le fichier final, mais seront plutôt affichées à l'écran, ce qui peut aider à trouver et résoudre les problèmes plus rapidement.

## Voir aussi
- [Documentation officielle Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guide de démarrage rapide Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Tutorial : Les redirections et les pipes avec Fish Shell](https://cmdchallenge.com/fish_shell_tutorial.html#pipe)