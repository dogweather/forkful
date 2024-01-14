---
title:    "PHP: Vérification de l'existence d'un répertoire"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous travaillez avec des fichiers et des dossiers dans un projet PHP, il peut être utile de vérifier si un dossier existe avant de le manipuler. Cela peut vous éviter des erreurs et des bugs potentiels.

## Comment

Pour vérifier si un dossier existe en utilisant PHP, vous pouvez utiliser la fonction intégrée `is_dir`. Elle vérifie si un chemin donné est un dossier et renvoie un booléen true ou false en conséquence.

```PHP
if (is_dir($chemin_dossier)) {
    echo "$chemin_dossier existe."; // Output: /mon_dossier existe.
} else {
    echo "$chemin_dossier n'existe pas.";
}

```

## Plongée en profondeur

Cette fonction utilise le système de fichiers du serveur pour réaliser cette vérification, elle est donc très efficace et rapide. Cependant, il est important de noter que la fonction `is_dir` ne vérifie que l'existence d'un dossier, pas sa disponibilité en écriture ou en lecture.

Il est également possible d'utiliser la fonction `file_exists` pour vérifier l'existence d'un dossier, mais elle vérifie également l'existence de fichiers, ce qui peut ne pas être idéal pour notre cas d'utilisation spécifique.

## Voir aussi 

- [Documentation officielle de la fonction is_dir en PHP](https://www.php.net/manual/fr/function.is-dir.php)
- [Exemple d'utilisation de la fonction is_dir](https://www.sololearn.com/Course/PHP/?ref=app)
- [Tutoriel sur la manipulation de fichiers et de dossiers en PHP](https://www.w3schools.com/php/php_file_open.asp)