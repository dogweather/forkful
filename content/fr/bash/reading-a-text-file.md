---
title:                "Bash: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi lire un fichier texte

Lire un fichier texte est une opération courante en programmation Bash. Que vous souhaitiez extraire des données, modifier des valeurs ou simplement afficher le contenu d'un fichier, cette compétence est essentielle pour tout programmeur Bash. Dans cet article, nous explorerons comment lire un fichier texte en utilisant des commandes Bash, ainsi que des astuces et des astuces pour une utilisation efficace.

## Comment le faire

Tout d'abord, nous devons comprendre que tout fichier est stocké sous forme de flux de caractères, y compris les fichiers texte. Pour lire un fichier texte en Bash, nous pouvons utiliser la commande `cat` suivie du nom du fichier:

```Bash
cat fichier.txt
```

Cela affichera tout le contenu du fichier sur la console. Cependant, parfois nous voulons extraire ou afficher une partie spécifique du fichier. Dans ce cas, nous pouvons utiliser la commande `head` pour afficher les premières lignes du fichier.

```Bash
head -n 10 fichier.txt
```

Cette commande affichera les 10 premières lignes du fichier. De même, la commande `tail` peut être utilisée pour afficher les dernières lignes du fichier.

```Bash
tail -n 5 fichier.txt
```

Pour extraire des données spécifiques d'un fichier, nous pouvons utiliser la commande `grep`. Cette commande recherche une chaîne de caractères spécifique dans le fichier et retourne toutes les lignes correspondantes.

```Bash
grep "mot-clé" fichier.txt
```

## Plongée en profondeur

En plus des commandes mentionnées ci-dessus, il existe plusieurs autres façons de lire un fichier texte en Bash. Par exemple, la commande `less` permet de parcourir un fichier en utilisant des touches de direction, rendant la navigation et la lecture des fichiers plus faciles.

```Bash
less fichier.txt
```

La commande `more` est similaire à `less` mais affiche le contenu page par page. De plus, certaines commandes Bash, comme `awk` et `sed`, peuvent être utilisées pour lire et modifier des fichiers texte.

Il est également important de noter que les fichiers texte peuvent contenir des caractères spéciaux, tels que les accents ou les caractères de nouvelle ligne, qui peuvent causer des problèmes lors de la lecture du fichier. Pour éviter cela, nous pouvons utiliser des options spécifiques pour ces commandes ou enregistrer le fichier au format UTF-8.

## Voir aussi

Pour en savoir plus sur la lecture des fichiers texte en Bash, vous pouvez consulter les ressources suivantes:

- [Tutorial Linux : les bases de Bash](https://www.tutoriel-linux.fr/commandesbash.html)
- [Les commandes Linux pour lire un fichier texte](https://www.lifewire.com/unix-command-to-display-text-file-2202923)
- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/bash.html#Manipulating-Files)