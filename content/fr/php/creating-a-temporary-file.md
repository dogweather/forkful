---
title:    "PHP: Création d'un fichier temporaire"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est une pratique courante en programmation PHP. Elle peut être utilisée dans de nombreux scénarios, tels que la gestion de fichiers temporaires lors de l'upload de fichiers ou la manipulation de données de grande taille. Les fichiers temporaires sont également utiles pour stocker des données sensibles et les supprimer après leur utilisation.

## Comment faire

Pour créer un fichier temporaire en PHP, vous pouvez utiliser la fonction `tempnam()` qui génère un nom unique pour le fichier temporaire et crée le fichier vide. Voici un exemple de code qui utilise `tempnam()` pour créer un fichier temporaire et y écrire du contenu :

```PHP
<?php
$filename = tempnam(sys_get_temp_dir(), 'prefix_');
$file = fopen($filename, 'w');
fwrite($file, 'Contenu du fichier temporaire');
fclose($file);
```

Ce code génère un fichier temporaire dans le répertoire système temporaire avec un préfixe spécifié. Vous pouvez également spécifier un suffixe en passant un troisième paramètre optionnel à la fonction `tempnam()`. N'oubliez pas de fermer le fichier après avoir écrit son contenu.

Une autre façon de créer un fichier temporaire est d'utiliser la fonction `tmpfile()`. Cette fonction crée un fichier temporaire dans le répertoire système temporaire et retourne un gestionnaire de fichier pour y écrire du contenu. Voici un exemple d'utilisation :

```PHP
<?php
$file = tmpfile();
fwrite($file, 'Contenu du fichier temporaire');
fclose($file);
```

Notez que `tmpfile()` gère la création et la suppression du fichier temporaire, vous n'avez donc pas besoin de spécifier un nom de fichier.

## Découverte approfondie

Il est important de noter que les fichiers temporaires créés avec `tempnam()` et `tmpfile()` sont automatiquement supprimés dès que la session PHP se termine. Si vous souhaitez supprimer manuellement le fichier temporaire, vous pouvez utiliser la fonction `unlink()` qui supprime un fichier du système de fichiers. Assurez-vous de vérifier si le fichier existe avant de le supprimer pour éviter toute erreur.

De plus, vous pouvez également utiliser la fonction `sys_get_temp_dir()` pour obtenir le chemin du répertoire temporaire par défaut sur votre système. Cela peut être utile si vous souhaitez stocker vos fichiers temporaires dans un emplacement spécifique.

## Voir aussi

- [Documentation PHP sur `tempnam()`](https://www.php.net/manual/fr/function.tempnam.php)
- [Documentation PHP sur `sys_get_temp_dir()`](https://www.php.net/manual/fr/function.sys-get-temp-dir.php)
- [Documentation PHP sur `unlink()`](https://www.php.net/manual/fr/function.unlink.php)
- [Documentation PHP sur `tmpfile()`](https://www.php.net/manual/fr/function.tmpfile.php)