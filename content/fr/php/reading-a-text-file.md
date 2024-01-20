---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?

Lire un fichier texte en PHP signifie extraire les informations contenues dans ce fichier et les mettre à la disposition du programme. Les programmeurs font cela pour récupérer des données qui peuvent être utilisées plus tard dans leur code.

## Comment faire :

Voici comment lire un fichier texte en PHP.

```PHP
$monFichier = fopen('monFichier.txt', 'r');
if ($monFichier) {
    while (($ligne = fgets($monFichier)) !== false) {
        echo $ligne;
    }
    fclose($monFichier);
} else {
    echo "Erreur lors de l'ouverture du fichier";
}
```

Dans cet exemple, `fopen` est utilisé pour ouvrir le fichier. `fgets` lit le fichier ligne par ligne, et chaque ligne est ensuite affichée à l'écran. Si une erreur se produit pendant l'ouverture du fichier, un message d'erreur est affiché.

## Plongée en profondeur :

La lecture de fichiers texte a commencé avec les premiers ordinateurs, qui utilisaient les fichiers texte pour stocker les données. Depuis lors, de nombreuses alternatives à la lecture de fichiers texte ont été développées, mais la lecture de fichiers texte reste une technique fondamentale en programmation.

En PHP, vous pouvez également utiliser `file_get_contents` pour lire tout le fichier à la fois, mais cette méthode peut poser problème si vous travaillez avec un fichier de grande taille.

De plus, il est important de noter que les fonctionnalités de lecture de fichiers peuvent varier en fonction de la configuration de votre serveur PHP. Par exemple, si l'option "allow_url_fopen" est désactivée, vous ne pourrez pas ouvrir de fichiers à partir d'URLs.

## Voir aussi :

Pour une exploration plus approfondie du sujet, consultez ces sources :
  
1. Manuel PHP officiel : [PHP: Fichier système - Manual](https://www.php.net/manual/fr/book.filesystem.php).
2. Stack Overflow : [Reading a text file in PHP - Stack Overflow](https://stackoverflow.com/questions/34519829/reading-a-text-file-in-php).
3. W3Schools : [PHP File Open/Read/Close - W3Schools](https://www.w3schools.com/php/php_file.asp).