---
title:                "Vérifier si un répertoire existe"
html_title:           "PHP: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?

Vérifier si un répertoire existe est une étape de programmation qui consiste à confirmer si un certain chemin de fichier mène à un répertoire existant. Les programmeurs le font pour prévenir les erreurs avant qu'elles ne surviennent.

## Comment faire :

Utilisez la fonction `is_dir` pour vérifier si un répertoire existe. Voyons un exemple simpliste :

```PHP
<?php
$dir = '/chemin/vers/repertoire';

if (is_dir($dir)) {
    echo "Le répertoire existe";
} else {
    echo "Le répertoire n'existe pas";
}
?>
```
Si le répertoire existe, cela afficherait "Le répertoire existe". Sinon, "Le répertoire n'existe pas" est renvoyé. 

## Approfondissement

D'un point de vue historique, la fonction `is_dir` est une partie intégrale de PHP depuis PHP 4. Avant cela, les programmeurs devaient utiliser des approches plus complexes pour vérifier si un répertoire existait.

Une alternative à `is_dir` pourrait être l'utilisation de `file_exists`. Cette fonction vérifie si un fichier ou un répertoire existe à partir d'un chemin donné. Cependant, il est à noter que `file_exists` renverra `true` même si le chemin indique un fichier et non un répertoire. Une utilisation prudente est donc essentielle. 

Quand `is_dir` est appelée, PHP effectue une recherche dans le système de fichiers pour voir si le répertoire spécifié existe. Si oui, elle retourne `true`; sinon, elle retourne `false`.

## Voir également

1. [Fonction is_dir - Documentation de PHP](https://www.php.net/manual/fr/function.is-dir.php)
2. [Fonction file_exists - Documentation de PHP](https://www.php.net/manual/fr/function.file-exists.php)
3. [Guide avancé sur les systèmes de fichiers avec PHP](https://www.php.net/manual/fr/book.filesystem.php)