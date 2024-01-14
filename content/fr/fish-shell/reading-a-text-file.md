---
title:                "Fish Shell: Lecture d'un fichier texte"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous utilisez Fish Shell pour votre programmation, il peut être très utile de savoir comment lire un fichier texte. Cela vous permettra de traiter facilement des fichiers de données et de manipuler les informations qu'ils contiennent.

## Comment faire

La syntaxe pour lire un fichier texte dans Fish Shell est simple. Tout d'abord, vous devez ouvrir un terminal et naviguer jusqu'au dossier contenant votre fichier texte. Ensuite, vous pouvez utiliser la commande `cat` pour afficher le contenu du fichier.

```Fish Shell
cd chemin/vers/le/dossier
cat fichier.txt
```

Cela affichera le contenu du fichier texte directement dans votre terminal. Si vous souhaitez enregistrer le contenu dans une variable Fish Shell, vous pouvez utiliser la commande `set` suivie du symbole de redirection `>`.

```Fish Shell
set fichier_contents (cat fichier.txt)
```

Vous pouvez également utiliser cette méthode pour lire un fichier texte ligne par ligne en utilisant la commande `while` et la fonction `read`.

```Fish Shell
while read ligne
    echo $ligne
end < fichier.txt
```

## Plongée plus profonde

Il existe de nombreuses options que vous pouvez utiliser avec la commande `cat` pour lire des fichiers texte dans Fish Shell. Par exemple, vous pouvez spécifier le nombre de lignes à afficher en utilisant l'option `-n`.

```Fish Shell
cat -n fichier.txt
```

De plus, vous pouvez utiliser l'option `-s` pour remplacer les lignes vides par une seule ligne. Cela peut être utile si vous travaillez avec de grands fichiers et que vous voulez en voir un aperçu rapide.

```Fish Shell
cat -s fichier.txt
```

Enfin, si vous souhaitez afficher uniquement une partie spécifique du fichier, vous pouvez utiliser l'option `-o` suivie du numéro de ligne de début et de fin.

```Fish Shell
cat -o 5-10 fichier.txt
```

## Voir aussi

- [Documentation officielle Fish Shell](https://fishshell.com/docs/current/)

- [Tutoriel Fish Shell](https://devdojo.com/blog/tutorials/fish-shell-tutorial)

- [Exemples pratiques d'utilisation de Fish Shell](https://www.binaryphile.com/fish/)