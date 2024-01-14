---
title:                "PHP: Création d'un fichier temporaire"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en PHP

La création de fichiers temporaires peut sembler inutile au premier abord, mais en réalité, cela peut être une technique très utile lors de la programmation en PHP. Les fichiers temporaires sont utilisés pour stocker des données temporaires ou pour effectuer des tâches temporaires, sans avoir à modifier les fichiers existants ou à en créer de nouveaux. Dans cet article, nous allons expliquer pourquoi la création de fichiers temporaires peut être utile dans vos projets de programmation en PHP.

## Comment créer un fichier temporaire en PHP

Pour créer un fichier temporaire en PHP, vous pouvez utiliser la fonction `tmpfile()`. Cette fonction crée un nouveau fichier vide et temporaire et renvoie son pointeur de fichier. Voici un exemple de code qui crée un fichier temporaire et y écrit quelques données :

```PHP
<?php
// Création d'un fichier temporaire
$monFichierTemp = tmpfile();

// Écriture de données dans le fichier
fwrite($monFichierTemp, "Ceci est un exemple de données");

// Lecture des données depuis le fichier temporaire
fseek($monFichierTemp, 0); // On se replace au début du fichier
echo fgets($monFichierTemp); // On lit les données et on les affiche

// Fermeture du fichier temporaire
fclose($monFichierTemp);
?>
```

Le code ci-dessus devrait produire la sortie suivante :

```PHP
Ceci est un exemple de données
```

Notez que le fichier temporaire est automatiquement supprimé lorsque vous fermez son pointeur de fichier en utilisant la fonction `fclose()`. Vous n'avez donc pas besoin de vous soucier de la suppression du fichier temporaire après son utilisation.

## Plus d'informations sur la création de fichiers temporaires en PHP

Il existe différentes manières de créer et de gérer des fichiers temporaires en PHP. Par exemple, vous pouvez utiliser la fonction `tmpnam()` pour obtenir le nom d'un fichier temporaire unique qui sera créé dans le répertoire de fichiers temporaires par défaut de votre système. Vous pouvez également utiliser la fonction `tempnam()` pour créer un fichier temporaire avec un nom personnalisé et dans un répertoire spécifique.

Il est également important de noter que les fichiers temporaires peuvent être utilisés dans différentes parties de votre code pour stocker des données temporaires ou pour effectuer des tâches temporaires, et qu'ils peuvent être supprimés à tout moment après leur utilisation. Les fichiers temporaires sont également très utiles pour tester certaines fonctionnalités de votre code avant de les implémenter définitivement dans les fichiers existants.

## Voir aussi

- [Documentation PHP sur les fichiers temporaires](https://www.php.net/manual/fr/book.filesystem.php)
- [Article sur la mise en oeuvre des fichiers temporaires en PHP](https://www.php.net/manual/fr/function.tmpfile.php)
- [Tutoriel sur la gestion des fichiers en PHP](https://www.tutorialspoint.com/php/php_files.htm)

Merci d'avoir lu cet article sur la création de fichiers temporaires en PHP. Nous espérons que cela vous sera utile dans vos futurs projets de programmation. N'hésitez pas à explorer davantage la documentation PHP pour découvrir d'autres fonctionnalités intéressantes et à les implémenter dans votre code. Bonne programmation !