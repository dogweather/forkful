---
title:                "Vérifier si un répertoire existe"
html_title:           "PHP: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est de vérifier si un certain répertoire existe. Cela peut être utile pour s'assurer que vous avez accès à un certain chemin de fichiers ou pour valider l'existence d'un répertoire avant de lancer une opération.

## Comment faire

Pour vérifier si un répertoire existe en PHP, nous pouvons utiliser la fonction `is_dir()`. Cette fonction prend en argument le chemin du répertoire et renvoie une valeur booléenne `true` si le répertoire existe, et `false` s'il n'existe pas.

```PHP
<?php
$directory = '/chemin/vers/mon/repertoire';
if (is_dir($directory)) {
    echo 'Le répertoire existe !';
} else {
    echo 'Le répertoire n\'existe pas.';
}
```

Si vous souhaitez également vérifier si le répertoire est accessible en écriture, vous pouvez utiliser la fonction `is_writable()`, qui renvoie également une valeur booléenne `true` ou `false`.

```PHP
<?php
if (is_dir($directory)) {
    if (is_writable($directory)) {
        echo 'Le répertoire est accessible en écriture.';
    } else {
        echo 'Le répertoire n\'est pas accessible en écriture.';
    }
} else {
    echo 'Le répertoire n\'existe pas.';
}
```

## Exploration approfondie

La fonction `is_dir()` utilise les fonctions système sous-jacentes pour vérifier l'existence du répertoire. Cela signifie que si le répertoire est inaccessible en raison de permissions ou d'autres problèmes, la fonction renverra toujours `false`, même s'il existe réellement.

Pour éviter cela, vous pouvez également utiliser la fonction `file_exists()` qui vérifie non seulement les répertoires, mais aussi les fichiers. Cela peut être utile si vous n'êtes pas certain du type de ressource que vous recherchez.

```PHP
<?php
$directory = '/chemin/vers/mon/repertoire';
if (file_exists($directory)) {
    echo 'Le répertoire ou fichier existe !';
} else {
    echo 'Le répertoire ou fichier n\'existe pas.';
}
```

Vous pouvez également utiliser la fonction `glob()` pour rechercher un répertoire ou un ensemble de fichiers correspondant à un modèle donné. Cette fonction renvoie un tableau contenant tous les chemins correspondants, ce qui peut être très utile pour des opérations plus avancées.

## Voir aussi

- La documentation officielle de PHP sur la fonction `is_dir()` : https://www.php.net/manual/fr/function.is-dir.php
- La documentation officielle de PHP sur la fonction `file_exists()` : https://www.php.net/manual/fr/function.file-exists.php
- La documentation officielle de PHP sur la fonction `glob()` : https://www.php.net/manual/fr/function.glob.php