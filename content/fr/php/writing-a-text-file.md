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

## Pourquoi

Écrire un fichier texte à l'aide du langage PHP est une compétence essentielle pour tout développeur web. Cela vous permet de stocker et d'afficher du contenu dynamiquement, ce qui est crucial dans la création de sites web interactifs et personnalisables.

## Comment faire

Pour écrire un fichier texte en utilisant PHP, vous devez suivre ces étapes simples :

```PHP
// Ouvrir un fichier en mode écriture
$file = fopen('monfichier.txt', 'w');

// Écrire du contenu dans le fichier
fwrite($file, "Bonjour le monde!");

// Fermer le fichier
fclose($file);
```

Vous pouvez également spécifier un chemin absolu pour votre fichier ou utiliser la constante `__DIR__` pour spécifier un chemin relatif au répertoire courant.

## Plongez plus profondément

Il existe plusieurs options que vous pouvez utiliser lors de l'écriture d'un fichier texte en PHP. Par exemple, vous pouvez spécifier le mode d'ouverture du fichier, ajouter ou remplacer du contenu existant, ou encore écrire des données d'un tableau dans le fichier.

Voici quelques exemples :

```PHP
// Écrire du contenu dans un fichier existant sans effacer le contenu existant
$file = fopen('monfichier.txt', 'a+');

// Écrire à partir d'un tableau
$data = array('Lundi', 'Mardi', 'Mercredi');
$file = fopen('jours.txt', 'w');
fwrite($file, implode("\n", $data));

// Ajouter du contenu à la fin d'un fichier
$file = fopen('monfichier.txt', 'a');
fwrite($file, "Continué...\n");

// Remplacer du contenu existant
$file = fopen('monfichier.txt', 'w');
fwrite($file, "Nouveau contenu");
```

Pour en savoir plus sur les différentes options et fonctionnalités disponibles lors de l'écriture d'un fichier texte en PHP, vous pouvez consulter la documentation officielle de PHP sur la fonction `fwrite()`.

## Voir aussi

- [Documentation officielle de PHP sur la fonction fwrite()](https://www.php.net/manual/fr/function.fwrite.php)
- [Guide complet pour écrire un fichier texte en PHP](https://www.php.net/manual/fr/function.fwrite.php)
- [Tutoriel vidéo sur l'écriture de fichiers texte en PHP](https://www.youtube.com/watch?v=DMuoH8CESxs)