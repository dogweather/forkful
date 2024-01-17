---
title:                "Ecrire un fichier texte"
html_title:           "PHP: Ecrire un fichier texte"
simple_title:         "Ecrire un fichier texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi écrire un fichier texte en PHP

## Qu'est-ce que c'est et pourquoi les programmeurs le font

Ecrire un fichier texte en PHP est une tâche courante pour les programmeurs. Cela consiste simplement à écrire du contenu dans un fichier texte, que ce soit du texte brut ou des données structurées au format de texte. Les programmeurs font cela pour stocker et manipuler des données à travers un langage de programmation, plutôt que de simplement les taper manuellement dans un fichier.

## Comment faire:

```PHP
<?php
// Ouvrir un fichier texte en écriture
$file = fopen("exemple.txt", "w") or die("Impossible d'ouvrir le fichier!");

// Ecrire du texte dans le fichier
$txt = "Bonjour le monde!";
fwrite($file, $txt);

// Fermer le fichier
fclose($file);

// Lire le contenu du fichier
echo file_get_contents("exemple.txt");
// Output: "Bonjour le monde!"
?>
```

## Plongeon dans le sujet:

Ecrire des fichiers texte en PHP remonte aux débuts du langage et est toujours une méthode efficace pour stocker et manipuler des données. Alternativement, les programmeurs peuvent également utiliser d'autres formats de fichiers tels que JSON ou CSV pour stocker et transférer des données. L'implémentation de l'écriture de fichiers texte en PHP est assez simple et ne nécessite pas de bibliothèques supplémentaires.

## Voir aussi:

- [PHP fopen() function](https://www.php.net/manual/fr/function.fopen.php)
- [PHP fwrite() function](https://www.php.net/manual/fr/function.fwrite.php)
- [PHP file_get_contents() function](https://www.php.net/manual/fr/function.file-get-contents.php)