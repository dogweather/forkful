---
title:    "Fish Shell: Trouver la longueur d'une chaîne de caractères"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

La détermination de la longueur d'une chaîne de caractères est une tâche courante en programmation. Que ce soit pour compter le nombre de lettres dans un mot ou le nombre de caractères dans une phrase, cette opération est souvent nécessaire pour manipuler et analyser des données. Dans cet article, nous allons vous montrer comment utiliser la commande Fish Shell pour trouver la longueur d'une chaîne de caractères.

## Comment faire

Nous allons utiliser la commande `string length` pour trouver la longueur d'une chaîne de caractères. Tout d'abord, ouvrez votre terminal Fish Shell et entrez la commande suivante :

```Fish Shell
string length "Bonjour"
```

Vous devriez voir une sortie de "7", car il y a 7 caractères dans le mot "Bonjour". Vous pouvez également utiliser cette commande avec des variables pour trouver la longueur d'une chaîne stockée dans une variable. Par exemple :

```Fish Shell
set mot "Bonjour"
string length $mot
```

Cela affichera également une sortie de "7". La commande `string length` peut être utilisée avec n'importe quelle chaîne de caractères, qu'il s'agisse d'un simple mot ou d'une phrase entière.

## Plongée en profondeur

Saviez-vous que la commande `string length` a une option `-r` qui permet de compter le nombre de caractères réels dans une chaîne, plutôt que le nombre de codepoints ? Les codepoints sont des codes numériques attribués à chaque caractère d'une langue, et la longueur d'une chaîne peut varier en fonction de la façon dont les caractères sont encodés. Si vous utilisez l'option `-r`, la commande `string length` ne comptera que les caractères réels et non les codepoints. Par exemple :

```Fish Shell
string length -r "é" 
```

Cela affichera une sortie de "1", alors qu'avec l'option `-r` cela afficherait "2". Cela peut être utile lorsque vous travaillez avec des chaînes de caractères multilingues.

## Voir aussi

- [Documentation de la commande Fish Shell string length](https://fishshell.com/docs/current/cmds/string-length.html)
- [Guide de Fish Shell pour les débutants](https://www.linux.com/training-tutorials/Basics_of_Linux/FOSS/#!/term/Fish%20Shell)
- [Tutoriels et ressources pour Fish Shell](https://fishshell.com/docs/current/project.html#tutorials)