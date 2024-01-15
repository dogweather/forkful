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

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez vous intéresser à créer un fichier temporaire en utilisant PHP ? Eh bien, la réponse est simple : les fichiers temporaires sont utiles lorsque vous devez stocker temporairement des données ou des informations qui ne sont pas destinées à être sauvegardées à long terme. Par exemple, si vous avez besoin de stocker des données en mémoire pendant le traitement d'une requête ou dans le cadre d'un script, créer un fichier temporaire peut être très pratique.

## Comment faire

La création d'un fichier temporaire en PHP est assez simple, grâce à la fonction native "tempnam()". Voici un exemple de code pour vous guider :

```PHP
<?php
// Création d'un fichier temporaire dans le répertoire courant
$filename = tempnam('.', 'tmp_');
// Écriture du contenu souhaité dans le fichier temporaire
file_put_contents($filename, 'Contenu de mon fichier temporaire');
// Lecture du contenu du fichier
echo file_get_contents($filename);
// Suppression du fichier temporaire
unlink($filename);
?> 
```

Output :

```
Contenu de mon fichier temporaire
```

Dans cet exemple, nous utilisons la fonction "tempnam()" qui prend deux paramètres : le répertoire dans lequel vous souhaitez créer le fichier (dans notre cas, le répertoire courant) et un préfixe pour le nom de fichier. Ensuite, nous utilisons les fonctions "file_put_contents()" et "file_get_contents()" pour écrire et lire le contenu du fichier temporaire. Enfin, la fonction "unlink()" est utilisée pour supprimer le fichier temporaire après son utilisation.

## Plongée en profondeur

Maintenant que vous savez comment créer un fichier temporaire en PHP, vous pouvez également vous demander : "Mais où ce fichier est-il stocké exactement ?". La réponse dépend du système d'exploitation que vous utilisez.

Sur les systèmes Linux, le fichier temporaire sera stocké dans le répertoire "/tmp". Sur les systèmes Windows, le fichier sera stocké dans le répertoire "%temp%". Dans les deux cas, le nom du fichier sera généré de manière aléatoire pour éviter tout conflit de noms de fichiers.

Il est également important de noter que la fonction "tempnam()" crée en fait un fichier vide, vous devrez donc utiliser les fonctions "file_put_contents()" ou "fwrite()" pour y écrire du contenu.

## Voir aussi

- [Documentation PHP sur la fonction tempnam()](https://www.php.net/manual/en/function.tempnam.php)
- [Explications sur les fichiers temporaires en PHP](https://www.php.net/manual/en/features.file-upload.php)