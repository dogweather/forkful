---
title:                "Lecture d'un fichier texte"
date:                  2024-01-20T17:54:49.490954-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Lire un fichier texte, c'est extraire son contenu pour l'utiliser dans votre script PHP. On fait ça pour accéder aux données, configurer des paramètres, ou lire des logs.

## How to: (Comment faire :)

Utilisons `file_get_contents()` pour lire un fichier entier :

```PHP
<?php
$content = file_get_contents('exemple.txt');
echo $content;
?>
```

Si `exemple.txt` contient "Bonjour les devs!", le script affichera :

```
Bonjour les devs!
```

Pour lire ligne par ligne, `fgets()` est votre ami :

```PHP
<?php
$file = fopen('exemple.txt', 'r');

while ($line = fgets($file)) {
  echo $line;
}

fclose($file);
?>
```

Avec le même contenu `exemple.txt`, la sortie sera identique.

## Deep Dive (Plongée profonde)

Historiquement, PHP permet les opérations sur fichiers en s'inspirant de C. La fonction `fopen()`, par exemple, ouvre un fichier, tandis que `fgets()` et `fread()` lisent les données.

En alternative, on a `file()` qui lit le fichier en un seul appel et renvoie un tableau. Pratique, mais consomme plus de mémoire pour les gros fichiers.

Il est important de gérer les droits d'accès et l'existence du fichier pour éviter des erreurs d'exécution. Utilisez `is_readable()` avant de lire.

## See Also (Voir aussi)

- La documentation officielle de PHP pour `file_get_contents()`: https://www.php.net/manual/fr/function.file-get-contents.php

- La documentation de `fgets()` : https://www.php.net/manual/fr/function.fgets.php

- Guide sur la gestion des fichiers en PHP : https://www.php.net/manual/fr/book.filesystem.php

Et voilà, maintenant vous savez l'essentiel sur la lecture de fichiers en PHP, c'est simple comme bonjour !
