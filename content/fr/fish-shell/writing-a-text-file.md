---
title:                "Fish Shell: Écriture d'un fichier texte"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte est une tâche courante pour les programmeurs, qu'il s'agisse de stocker des données, de configurer des paramètres ou d'écrire du code. Avec Fish Shell, il existe une façon simple et efficace d'écrire des fichiers texte directement à partir de votre terminal. Dans cet article, nous allons vous montrer comment le faire.

## Comment faire

Tout d'abord, ouvrez votre terminal Fish Shell et déplacez-vous dans le dossier où vous souhaitez créer votre fichier texte. Ensuite, tapez la commande suivante:

```Fish Shell
echo "Bonjour le monde" > mon_fichier.txt
```

Cette commande va créer un nouveau fichier texte appelé "mon_fichier.txt" et y ajouter la phrase "Bonjour le monde". Vous pouvez utiliser n'importe quelle phrase ou texte à la place, cela remplacera simplement ce qui est déjà présent dans le fichier ou le créera s'il n'existe pas encore.

Maintenant, pour ajouter du contenu supplémentaire à votre fichier existant, vous pouvez utiliser la commande suivante:

```Fish Shell
echo "Ceci est une nouvelle ligne ajoutée" >> mon_fichier.txt
```

Cette commande ajoutera la phrase "Ceci est une nouvelle ligne ajoutée" à la fin de votre fichier texte existant.

Il est également possible de créer un fichier texte vide à partir du terminal en utilisant la commande suivante:

```Fish Shell
touch nouveau_fichier.txt
```

Cette commande créera un nouveau fichier vide appelé "nouveau_fichier.txt". Vous pouvez ensuite l'ouvrir dans un éditeur de texte pour y ajouter du contenu.

## Plongée en profondeur

Si vous souhaitez écrire un fichier texte plus complexe, avec plusieurs lignes, vous pouvez utiliser l'éditeur de texte Vi intégré à Fish Shell. Pour ouvrir un fichier texte existant, utilisez la commande suivante:

```Fish Shell
vi mon_fichier.txt
```

Cela ouvrira le fichier dans l'éditeur de texte Vi, où vous pourrez utiliser différentes commandes pour écrire et modifier votre texte. Pour ajouter du texte, utilisez la touche "i" pour entrer en mode insertion, puis écrivez votre texte. Une fois que vous avez terminé, utilisez "Esc" pour revenir en mode commande, puis écrivez ":wq" pour enregistrer et quitter le fichier.

Pour apprendre plus en détail sur l'utilisation de Vi, vous pouvez consulter notre article sur le sujet [ici](lien vers l'article sur l'utilisation de Vi).

## Voir aussi

- [Documentation de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guide d'utilisation de Vi](lien vers l'article sur l'utilisation de Vi)
- [Autres commandes utiles pour écrire des fichiers texte avec Fish Shell](lien vers un autre article sur les commandes utiles pour écrire des fichiers texte avec Fish Shell)