---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire un fichier texte, c'est sauvegarder des données dans un format simple et lisible. Les programmeurs le font pour enregistrer des configurations, des logs ou échanger des données entre applications.

## How to:
Pour écrire dans un fichier texte en PHP, on utilise `file_put_contents()` ou un combo `fopen()`, `fwrite()`, et `fclose()`.

```PHP
<?php
$texte = "Salut tout le monde!\n";
file_put_contents('bonjour.txt', $texte);
?>
```

Résultat: Crée ou remplace `bonjour.txt` avec "Salut tout le monde!".

Ou, pour plus de contrôle:

```PHP
<?php
$fichier = fopen('hello.txt', 'a'); // 'a' pour append (ajouter)
if ($fichier) {
    fwrite($fichier, $texte);
    fclose($fichier);
}
?>
```

Résultat: Ajoute "Salut tout le monde!" à la fin de `hello.txt`.

## Deep Dive
Historiquement, `fopen()` et ses amis étaient les seules options en PHP pour écrire dans des fichiers. `file_put_contents()` est plus récent et plus simple pour des cas d'usage basiques.

Les alternatives incluent l'usage de bibliothèques comme SPL (Standard PHP Library) pour manipuler des fichiers avec des objets.

Il faut gérer les permissions de fichiers : assurez-vous que votre script a le droit d'écrire dans le dossier cible.

## See Also
- Documentation PHP sur la gestion de fichiers : [php.net/manual/fr/book.filesystem.php](https://www.php.net/manual/fr/book.filesystem.php)
- Tuto complet PHP sur `fopen()` : [php.net/manual/fr/function.fopen.php](https://www.php.net/manual/fr/function.fopen.php)
- Infos sur les flags de `fopen()` : [php.net/manual/fr/function.fopen.php](https://www.php.net/manual/fr/function.fopen.php#refsect1-function.fopen-parameters)
