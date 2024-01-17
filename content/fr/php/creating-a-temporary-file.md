---
title:                "Créer un fichier temporaire"
html_title:           "PHP: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Titre:

## Qu'est-ce que c'est et pourquoi le faire?

Créer un fichier temporaire en programmation PHP implique de créer un fichier qui sera supprimé automatiquement après son utilisation. Les programmeurs le font souvent lorsqu'ils ont besoin de stocker temporairement des données ou de manipuler des fichiers sans affecter les fichiers existants.

## Comment faire :

Voici un exemple de code pour créer un fichier temporaire en PHP :

```PHP
$file = tmpfile();
fwrite($file, "Ce texte sera écrit dans le fichier temporaire.");
echo ftell($file); // affiche la position actuelle dans le fichier
rewind($file); // remet le pointeur au début du fichier
echo fread($file, filesize("data.txt")); // affiche le contenu du fichier
fclose($file); // ferme le fichier temporaire et le supprime automatiquement
```

L'exemple ci-dessus utilise la fonction `tmpfile()` pour créer un fichier temporaire et la fonction `fwrite()` pour y écrire des données. La fonction `ftell()` affiche la position actuelle dans le fichier et la fonction `rewind()` remet le pointeur au début du fichier. Enfin, la fonction `fclose()` ferme le fichier temporaire et le supprime automatiquement.

## Profonde plongée :

La pratique de créer des fichiers temporaires en programmation peut être utile lorsque l'on travaille avec des fichiers volumineux ou sensibles. En effet, en utilisant des fichiers temporaires, on évite les risques de corruption ou de perte de données.

Il existe également d'autres moyens de manipuler des fichiers en PHP, tels que les fonctions `fopen()` et `file_put_contents()`, mais ils peuvent avoir des effets différents sur les fichiers existants. La création de fichiers temporaires est donc une solution plus sûre et plus efficace dans certains cas.

En ce qui concerne l'implémentation, la création de fichiers temporaires en PHP peut se faire en utilisant la bibliothèque standard `sys_tempnam()` ou en utilisant la classe `SplFileObject`.

## Voir aussi :

- [La documentation officielle de PHP sur les fichiers temporaires](https://www.php.net/manual/fr/function.tmpfile.php)
- [Un tutoriel sur la création de fichiers temporaires en PHP](https://www.geeksforgeeks.org/php-tmpfile-function/)
- [Une comparaison entre différentes méthodes de manipulation de fichiers en PHP](https://www.php.net/manual/fr/function.fopen.php)