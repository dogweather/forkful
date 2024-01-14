---
title:                "PHP: Ecrire un fichier texte"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire un fichier texte est une compétence de base pour tout programmeur PHP. Cela permet de stocker et de manipuler des données, ce qui est essentiel pour développer des applications web ou des scripts. Dans cet article, nous allons vous montrer comment écrire un fichier texte en PHP et quelques astuces pour aller plus loin.

## Comment faire

Pour écrire un fichier texte en PHP, nous allons utiliser la fonction `fwrite()`. Cette fonction prend deux paramètres : le chemin du fichier et le contenu à écrire. Voici un exemple de code pour écrire dans un fichier nommé `texte.txt` :

```
<?php
$file = fopen("texte.txt", "w");
fwrite($file, "Ceci est un exemple de contenu à écrire dans notre fichier.");
fclose($file);
```

Si tout se passe bien, le fichier `texte.txt` sera créé à côté de votre fichier PHP et il contiendra le texte que nous avons spécifié. Vous pouvez également utiliser la fonction `file_put_contents()` qui effectue les mêmes opérations, mais en une seule ligne de code :

```
<?php
file_put_contents("texte.txt", "Ceci est un exemple de contenu à écrire dans notre fichier.");
```

Vous pouvez aussi ajouter du contenu à un fichier existant en utilisant le mode d'écriture `a` (pour "append") :

```
<?php
$file = fopen("texte.txt", "a");
fwrite($file, "Ceci est un nouveau contenu à ajouter au fichier.");
fclose($file);
```

## Plongée en profondeur

Maintenant que vous savez comment écrire dans un fichier texte en PHP, voici quelques astuces pour aller plus loin :

- Vous pouvez utiliser la fonction `file_exists()` pour vérifier si un fichier existe déjà avant d'écrire dedans.
- Vous pouvez également utiliser le mode d'ouverture `r+` pour lire et écrire dans le même fichier.
- La fonction `fputcsv()` vous permet d'écrire des données dans un fichier CSV (format de données tabulées).
- N'oubliez pas de fermer le fichier avec la fonction `fclose()` après avoir fini d'écrire.

## Voir aussi

- [La documentation PHP sur la fonction fwrite()](https://www.php.net/manual/fr/function.fwrite.php)
- [Un tutoriel complet sur la manipulation de fichiers en PHP](https://www.grafikart.fr/tutoriels/php/fichiers-156)

Félicitations, vous savez maintenant comment écrire dans un fichier texte en PHP ! Utilisez ces connaissances pour manipuler et stocker des données dans vos prochains projets. N'hésitez pas à consulter la documentation et à explorer d'autres fonctions liées à la manipulation de fichiers en PHP pour enrichir vos compétences en programmation. Bonne écriture !