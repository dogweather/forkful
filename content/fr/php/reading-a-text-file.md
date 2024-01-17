---
title:                "Lecture d'un fichier texte"
html_title:           "PHP: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est et pourquoi le lire?

Lorsque vous programmez avec PHP, vous pouvez avoir besoin de lire des fichiers de texte pour récupérer des données ou pour les afficher à l'écran. Lire un fichier de texte consiste simplement à ouvrir un fichier contenant du texte et à en extraire les informations.

Les programmeurs peuvent avoir besoin de lire des fichiers de texte pour une variété de raisons, telles que la récupération de données d'utilisateurs, la création de rapports ou la manipulation de grandes quantités de données.

Comment faire:

Pour lire un fichier de texte en utilisant PHP, vous pouvez utiliser la fonction `file_get_contents()`. Cette fonction ouvre le fichier spécifié et retourne son contenu sous forme de chaîne de caractères. Vous pouvez également utiliser la fonction `fopen()` pour ouvrir un fichier et ensuite utiliser la fonction `fgets()` pour lire chaque ligne du fichier.

```
// Exemple d'utilisation de file_get_contents()
$texte = file_get_contents('fichier.txt');

echo $texte; // Affiche le contenu du fichier
```

```
// Exemple d'utilisation de fgets()
$fichier = fopen('fichier.txt', 'r');

// Lit chaque ligne du fichier et l'affiche à l'écran
while(!feof($fichier)) {
    echo fgets($fichier) . "<br>";
}

fclose($fichier); // Ferme le fichier
```

Plongée plus profonde:

La lecture de fichiers de texte est une fonctionnalité courante dans de nombreux langages de programmation, et PHP ne fait pas exception. Cependant, avant la sortie de PHP 5 en 2004, il n'était pas possible de lire des fichiers texte en une seule ligne (comme dans l'exemple `file_get_contents()` ci-dessus). Les développeurs devaient donc utiliser des fonctions plus complexes pour y parvenir.

Il existe également des alternatives à la fonction `file_get_contents()`, telles que `fread()` et `file()`, qui offrent plus de flexibilité dans la gestion des fichiers et la récupération des données.

Pour ceux qui souhaitent explorer davantage les possibilités de manipulation de fichiers en PHP, il existe également des extensions telles que Filesystem, qui fournit des fonctions de haut niveau pour créer, modifier ou supprimer des fichiers et des dossiers.

Voir aussi:

- Documentation officielle PHP sur la fonction `file_get_contents()`: https://www.php.net/manual/fr/function.file-get-contents.php
- Documentation officielle PHP sur la fonction `fopen()`: https://www.php.net/manual/fr/function.fopen.php
- Documentation officielle PHP sur la fonction `fgets()`: https://www.php.net/manual/fr/function.fgets.php
- Extension Filesystem de PHP: https://www.php.net/manual/fr/book.filesystem.php