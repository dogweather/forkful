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

## Qu'est-ce que c'est et pourquoi?
Vérifier si un répertoire existe est une fonctionnalité importante pour les développeurs PHP. Cette vérification permet de s'assurer qu'un fichier souhaité soit présent avant d'y accéder ou avant de le créer. Cela permet également d'éviter les erreurs et les interruptions de processus lors de l'exécution d'un script.

## Comment faire:
Utiliser la fonction `file_exists()` pour vérifier si un répertoire existe. Voici un exemple de code :

```PHP
if (file_exists("/chemin/vers/le/répertoire")){
    echo "Le répertoire existe.";
} else{
    echo "Le répertoire n'existe pas.";
}
```

La sortie de ce code dépendra du répertoire donné en paramètre. Si le répertoire existe, le message "Le répertoire existe." sera affiché, sinon le message "Le répertoire n'existe pas." sera affiché.

## Zoom sur:
#### Contexte historique:
La vérification de l'existence d'un répertoire est une fonctionnalité couramment utilisée dans les langages de programmation, y compris en PHP. Elle a été ajoutée dès la première version de PHP et a subi plusieurs améliorations au fil des versions suivantes.

#### Alternatives:
Une alternative à la fonction `file_exists()` est la fonction `is_dir()` qui permet également de vérifier si un répertoire existe. La différence est que `is_dir()` renverra `true` uniquement si le chemin donné est un répertoire.

#### Détails de l'implémentation:
La fonction `file_exists()` utilise le cache I-nœud de Linux pour vérifier si un fichier ou un répertoire existe. Si le fichier ou le répertoire est présent dans le cache, la fonction renverra `true` immédiatement. Sinon, elle vérifiera physiquement si le fichier ou le répertoire existe. Ceci permet d'optimiser les performances lors de la vérification de l'existence de plusieurs fichiers ou répertoires.

## Voir aussi:
- Documentation officielle de la fonction `file_exists()`: https://www.php.net/manual/en/function.file-exists.php
- Documentation officielle de la fonction `is_dir()`: https://www.php.net/manual/en/function.is-dir.php
- Article sur la gestion des erreurs en PHP: [Gestion des erreurs en PHP: Une introduction pour les débutants](https://www.kernelverse.com/posts/errors-php-beginners/)