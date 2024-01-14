---
title:                "PHP: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi 

Il peut y avoir plusieurs raisons pour lesquelles vous pourriez avoir besoin de créer un fichier temporaire lors de la programmation en PHP. Par exemple, vous pourriez avoir besoin d'enregistrer temporairement des données lors du traitement d'une requête, ou utiliser un fichier temporaire pour stocker des informations sensibles de manière sécurisée.

# Comment faire

Voici un exemple simple de code PHP pour créer un fichier temporaire et y écrire du contenu :

```PHP
<?php
// Créer un fichier temporaire
$fichier = tmpfile();

// Écrire du contenu dans le fichier
fwrite($fichier, 'Ceci est un fichier temporaire.');

// Fermer le fichier
fclose($fichier);
?>
```

Le fichier temporaire sera automatiquement supprimé une fois que la fonction `tmpfile()` sera appelée.

# Plongée profonde 

Créer un fichier temporaire peut sembler simple, mais il existe plusieurs options et paramètres à considérer pour en faire usage de manière optimale.

Par exemple, la fonction `tmpfile()` renvoie un pointeur vers le fichier temporaire, mais si vous avez besoin de stocker le chemin du fichier, vous pouvez utiliser `tempnam()` qui renverra un nom de fichier unique.

De plus, vous pouvez spécifier un répertoire pour stocker les fichiers temporaires avec la fonction `sys_get_temp_dir()` et utiliser la constante `FILE_APPEND` pour ajouter du contenu à un fichier temporaire existant.

# Voir aussi 

- [Documentation officielle de PHP pour la création de fichiers temporaires](https://www.php.net/manual/fr/function.tmpfile.php)
- [Article sur la sécurisation des fichiers temporaires en PHP](https://www.techopedia.com/2/28586/programming/php/temporary-files-security-in-php)