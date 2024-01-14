---
title:                "PHP: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte peut sembler une tâche simple et banale, mais c'est en fait une compétence importante à maîtriser pour tout développeur PHP. Les fichiers texte sont souvent utilisés pour stocker des données textuelles, tels que des configurations ou des journaux de données, et peuvent être manipulés et lus par d'autres programmes. Par conséquent, en maîtrisant l'écriture de fichiers texte en PHP, vous pouvez améliorer vos compétences en manipulation et en stockage de données, ce qui peut être très utile dans vos projets futurs.

## Comment faire

Pour écrire un fichier texte en PHP, vous pouvez utiliser la fonction `file_put_contents()`. Cette fonction prend deux paramètres : le premier est le nom du fichier dans lequel vous souhaitez écrire, et le second est la chaîne de caractères que vous souhaitez écrire dans le fichier. Voici un exemple de code :

```PHP
$file = "monfichier.txt";
$data = "Ceci est un exemple de données que nous pouvons écrire dans le fichier."

file_put_contents($file, $data);
```

Ce code créera un fichier texte appelé "monfichier.txt" s'il n'existe pas déjà, et y écrira la chaîne de caractères spécifiée dans la variable `$data`. Vous pouvez également spécifier un troisième paramètre optionnel pour cette fonction, qui est le mode d'écriture du fichier. Par exemple, si vous voulez ajouter des données à un fichier existant sans écraser les données existantes, vous pouvez utiliser le mode `FILE_APPEND` comme ceci :

```PHP
$file = "monfichier.txt";
$data = "Nouvelles données ajoutées au fichier.";

file_put_contents($file, $data, FILE_APPEND);
```

## Plongée en profondeur

Il est important de noter que la fonction `file_put_contents()` remplacera tout le contenu du fichier si celui-ci existe déjà. Pour éviter cela, vous pouvez utiliser d'autres fonctions telles que `fopen()` et `fwrite()` pour écrire des données dans un fichier sans écraser le contenu existant. Vous pouvez également utiliser des boucles et des conditions pour gérer et formater les données que vous souhaitez écrire dans le fichier.

En plus d'écrire des fichiers texte, vous pouvez également les lire en utilisant des fonctions telles que `file_get_contents()` ou `fread()`. Vous pouvez également manipuler la position du pointeur de lecture avec la fonction `fseek()`, ce qui peut être utile pour parcourir un fichier avec précision.

## Voir aussi

- [Documentation PHP sur la fonction file_put_contents](https://www.php.net/manual/fr/function.file-put-contents.php)
- [Exemple d'utilisation de fwrite() et fopen()](https://www.php.net/manual/fr/function.fwrite.php)
- [Documentation PHP sur les fonctions de manipulation de fichiers](https://www.php.net/manual/fr/ref.filesystem.php)