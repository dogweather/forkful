---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?
Créer un fichier temporaire, c'est tout simplement créer un fichier de stockage provisoire. Les programmeurs le font pour stocker des données de façon temporaire sans saturer la mémoire vive de l'ordinateur.

## Comment faire:
Voici comment on peut créer un fichier temporaire en PHP. Notez que l'emplacement du fichier peut varier selon le système d'exploitation.

```PHP
<?php
// Créer un fichier temporaire
$tempFile = tmpfile();

// Écrire quelques données dans le fichier
fwrite($tempFile, "Hello, Monde!");

// Lire les données du fichier
rewind($tempFile);
echo fread($tempFile, 1024);

// Fermer le fichier temporaire, il sera supprimé
fclose($tempFile);
?>
```

Le code ci-dessus affiche `Hello, Monde!`.

## Plongeons un peu plus profondément
Historiquement, l'idée de créer des fichiers temporaires remonte aux premiers jours de la programmation. Cela permettait d'économiser la mémoire vive qui était alors un bien précieux. Aujourd'hui, même si la mémoire vive est moins limitante, l'utilisation des fichiers temporaires reste une pratique courante pour gérer efficacement les ressources.

Il existe des alternatives à la création de fichiers temporaires, par exemple l'utilisation de bases de données en mémoire (telles que Redis ou Memcached) pour le stockage temporaire de données. Cependant, dans certains cas, la création d'un fichier temporaire peut être plus appropriée.

En ce qui concerne les fichiers temporaires en PHP, l'implémentation est gérée par le système d'exploitation sous-jacent. Sur la plupart des systèmes Unix, le fichier sera stocké dans le répertoire /tmp.

## Voir aussi
Visitez les liens ci-dessous pour plus d'informations:
- [Documentation PHP officielle sur tmpfile()](http://php.net/manual/fr/function.tmpfile.php)
- [Guide sur l'utilisation des fichiers temporaires](https://www.geekhideout.com/urlcode.shtml)