---
date: 2024-01-20 17:41:04.515571-07:00
description: "Comment faire : Cr\xE9er un fichier temporaire en PHP est simple et\
  \ direct. Utilisez la fonction `tmpfile()` pour ouvrir et cr\xE9er automatiquement\
  \ un fichier\u2026"
lastmod: '2024-03-13T22:44:57.899847-06:00'
model: gpt-4-1106-preview
summary: "Cr\xE9er un fichier temporaire en PHP est simple et direct."
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

## Comment faire :
Créer un fichier temporaire en PHP est simple et direct. Utilisez la fonction `tmpfile()` pour ouvrir et créer automatiquement un fichier temporaire en mode écriture et lecture (w+).

```php
<?php
// Création du fichier temporaire
$temp = tmpfile();
if ($temp === false) {
    die('Impossible de créer un fichier temporaire.');
}

// Écriture de quelque chose dans le fichier temporaire
fwrite($temp, "Stockons des données temporairement!");

// Allons à la position initiale pour lire le contenu
rewind($temp);

// Lire et afficher le contenu du fichier
echo fread($temp, 1024);

// Fermeture et suppression automatique du fichier temporaire
fclose($temp);
```
Sortie:
```
Stockons des données temporairement!
```

## Plongée Profonde :
Historiquement, la gestion des fichiers temporaires est cruciale pour éviter de perdre des données importantes durant des coupures de courant ou des plantages. Les fichiers temporaires étaient souvent stockés dans des dossiers spécifiques comme `/tmp` sous Unix.

Alternativement à `tmpfile()`, PHP propose `tempnam()` pour créer un nom de fichier temporaire unique et `fopen()` pour ouvrir le fichier si vous avez besoin de plus de contrôle, comme la persistance du fichier après la fin du script.

L'utilisation de `tmpfile()` est préférée car elle gère mieux la sécurité, en s’assurant que les fichiers temporaires soient effacés après l’utilisation, ce qui aide à prévenir toute fuite de données.

## Voir Aussi :
- La documentation PHP sur les fichiers temporaires : [php.net/manual/fr/function.tmpfile.php](https://www.php.net/manual/fr/function.tmpfile.php)
- Un guide sur la gestion des fichiers en PHP : [php.net/manual/fr/book.filesystem.php](https://www.php.net/manual/fr/book.filesystem.php)
- Sécurité des fichiers temporaires : [owasp.org/index.php/Insecure_Temporary_File](https://owasp.org/index.php/Insecure_Temporary_File)
