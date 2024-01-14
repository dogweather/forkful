---
title:                "PHP: Lecture d'un fichier texte"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lecture de fichiers texte en PHP - pourquoi est-ce important pour les développeurs ? Les fichiers texte sont souvent utilisés pour stocker des informations et des données dans de nombreux projets de développement. Il est donc essentiel de savoir comment lire ces fichiers en PHP pour pouvoir manipuler les données stockées à l'intérieur.

## Comment Faire
Voici un exemple de code de base pour ouvrir et lire un fichier texte en PHP :

```PHP
<?php
    // Ouverture du fichier texte en mode lecture
    $fichier = fopen("monfichier.txt", "r");

    // Boucle pour parcourir chaque ligne du fichier
    while(!feof($fichier)){
        // Lecture de la ligne
        $ligne = fgets($fichier);
        // Affichage de la ligne
        echo $ligne. "<br/>";
    }

    // Fermeture du fichier
    fclose($fichier);
?>
```

Lorsque nous exécutons ce code, il affiche chaque ligne du fichier texte et les sépare avec une balise HTML `<br/>`. Vous pouvez également utiliser la fonction `file_get_contents()` pour stocker le contenu du fichier dans une variable et l'afficher plus tard.

## Deep Dive
Cette section est facultative, mais peut être utile pour ceux qui souhaitent en savoir plus sur la lecture de fichiers texte en PHP. En plus des fonctions `feof()` et `fgets()` utilisées dans l'exemple ci-dessus, il existe d'autres fonctions utiles comme `fgetc()` pour lire un caractère à la fois, `feof()` pour vérifier si la fin du fichier est atteinte, et `rewind()` pour revenir au début du fichier.

Il est également important de noter que lors de la lecture de fichiers texte, il est important de spécifier le chemin correct du fichier si celui-ci n'est pas situé dans le même dossier que votre fichier PHP. Vous pouvez utiliser des chemins absolus ou des chemins relatifs pour accéder au fichier.

## Voir Aussi
- [Documentation PHP sur la lecture de fichiers](https://www.php.net/manual/fr/function.fopen.php)
- [Tutoriel vidéo sur la lecture de fichiers en PHP](https://www.youtube.com/watch?v=79Dji5YAquk)
- [Exemples pratiques de lecture de fichiers texte en PHP](https://www.tutorialrepublic.com/php-tutorial/php-file-reading.php)