---
title:                "Fish Shell: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire du code peut sembler intimidant pour certaines personnes, mais cela peut être incroyablement utile et gratifiant. Écrire un fichier texte peut être un excellent moyen de stocker et d'organiser des informations pour une utilisation future. Dans cet article, nous allons explorer comment écrire un fichier texte avec Fish Shell.

## Comment faire

La première étape consiste à ouvrir un terminal et à ouvrir Fish Shell. Une fois que vous y êtes, vous pouvez commencer à écrire votre code. Tout d'abord, nous voulons créer notre fichier texte. Nous utilisons la commande `touch` pour cela:

```Fish Shell
touch fichier.txt
```

Cela va créer un nouveau fichier appelé "fichier.txt". Ensuite, nous pouvons utiliser la commande `echo` pour écrire des informations dans notre fichier. Par exemple, si nous voulons ajouter la phrase "Bonjour le monde !" dans notre fichier, nous utiliserions la commande suivante:

```Fish Shell
echo "Bonjour le monde !" > fichier.txt
```

Cela va écrire la phrase dans notre fichier et la remplacer par tout ce qu'il y avait auparavant. Si nous voulons simplement ajouter une nouvelle ligne de texte sans écraser l'ancienne, nous pouvons utiliser la commande `echo` suivante:

```Fish Shell
echo "Salut tout le monde !" >> fichier.txt
```

Cela ajoutera la phrase sur une nouvelle ligne dans notre fichier. Une fois que nous avons terminé d'écrire dans notre fichier, nous pouvons l'ouvrir pour voir le résultat final. Nous utilisons la commande `cat` pour cela:

```Fish Shell
cat fichier.txt
```

Cela affichera tout le contenu de notre fichier dans le terminal.

## Plongée profonde

Maintenant que nous savons comment écrire un fichier texte, voyons quelques astuces supplémentaires pour le rendre encore plus utile. Tout d'abord, si nous voulons écrire du texte sur plusieurs lignes, nous pouvons utiliser la commande `printf` à la place de `echo`. Par exemple, si nous voulons écrire un paragraphe dans notre fichier, nous utiliserions la commande suivante:

```Fish Shell
printf "Ceci est un paragraphe.\nIl contient plusieurs lignes de texte.\n" > fichier.txt
```

Nous avons ajouté le caractère `\n` à la fin de chaque ligne pour indiquer que nous voulons passer à la ligne suivante. Une autre astuce utile est de créer un fichier vide avec la commande `touch` et de l'utiliser comme base pour écrire un nouveau fichier. Par exemple, si nous avons déjà un fichier appelé "base.txt" avec une structure spécifique qui nous convient, nous pouvons utiliser la commande suivante pour en créer une copie nommée "nouveau_fichier.txt":

```Fish Shell
cp base.txt nouveau_fichier.txt
```

Cela créera une copie exacte de notre fichier "base.txt" qui peut être utilisée comme point de départ pour écrire un nouveau fichier.

## Voir aussi

- [Documentation de Fish Shell](https://fishshell.com/docs/current/)
- [Guide de démarrage rapide de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Liste des commandes Fish Shell](https://fishshell.com/docs/current/cmds.html)