---
title:    "PHP: Lecture d'un fichier texte"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

Il est très courant dans la programmation PHP de devoir lire des fichiers texte, que ce soit pour récupérer des données ou pour les afficher à l'utilisateur. Dans cet article, nous allons explorer les différentes façons de lire un fichier texte en utilisant PHP.

## Comment faire

Pour lire un fichier texte en PHP, nous utiliserons la fonction `file_get_contents()` qui permet de récupérer le contenu d'un fichier sous forme d'une chaîne de caractères. Voici un exemple:

```PHP
<?php
// Ouvre le fichier "texte.txt"
$texte = file_get_contents("texte.txt");

// Affiche le contenu du fichier
echo $texte;
```

Si vous voulez récupérer chaque ligne du fichier, vous pouvez utiliser la fonction `file()` qui retourne un tableau contenant chaque ligne du fichier. Par exemple:

```PHP
<?php
// Ouvre le fichier "texte.txt"
$lignes = file("texte.txt");

// Parcourt chaque ligne et affiche son contenu
foreach($lignes as $ligne){
    echo $ligne;
}
```

Enfin, il est important de bien gérer les erreurs lors de la lecture d'un fichier. Pour cela, nous pouvons utiliser la fonction `file_exists()` pour vérifier si le fichier existe avant de le lire.

Voici un exemple complet avec gestion des erreurs:

```PHP
<?php
// Vérifie si le fichier existe
if(file_exists("texte.txt")){
    // Ouvre le fichier
    $lignes = file("texte.txt");

    // Parcourt chaque ligne et affiche son contenu
    foreach($lignes as $ligne){
        echo $ligne;
    }
} else {
    // Affiche un message d'erreur si le fichier n'existe pas
    echo "Le fichier n'existe pas";
}
```

## Exploration en profondeur

Maintenant que nous savons comment lire un fichier texte en PHP, il est important de comprendre que différentes options peuvent être ajoutées à ces fonctions. Par exemple, il est possible de préciser le mode d'ouverture du fichier (`"r"` pour lecture seule, `"w"` pour écriture, etc.), d'ajouter des paramètres supplémentaires pour gérer les retours à la ligne (`FILE_IGNORE_NEW_LINES` par exemple), ou encore de préciser un délimiteur pour la fonction `file()`.

En utilisant ces options, il est possible d'adapter la lecture du fichier en fonction de nos besoins et de mieux gérer les différentes données qu'il contient.

## Voir aussi

- [Documentation de PHP sur `file_get_contents()`](https://www.php.net/manual/fr/function.file-get-contents.php)
- [Documentation de PHP sur `file()`](https://www.php.net/manual/fr/function.file.php)
- [Documentation de PHP sur `file_exists()`](https://www.php.net/manual/fr/function.file-exists.php)