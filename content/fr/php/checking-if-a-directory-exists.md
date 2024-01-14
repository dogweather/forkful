---
title:                "PHP: Vérification de l'existence d'un répertoire"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

La vérification de l'existence d'un répertoire est une étape importante dans la programmation PHP. Cela permet de s'assurer que le répertoire cible existe avant d'y effectuer des opérations telles que l'écriture de fichiers ou la lecture de données. Ainsi, cela évite les erreurs et les plantages inattendus.

## Comment faire

Pour vérifier si un répertoire existe en PHP, nous pouvons utiliser la fonction `file_exists()`. Cette fonction prend en paramètre le chemin du répertoire et retourne `true` si le répertoire existe ou `false` s'il n'existe pas. Voici un exemple de code :

```PHP
if (file_exists("chemin/vers/repertoire")) {
    echo "Le répertoire existe.";
} else {
    echo "Le répertoire n'existe pas.";
}
```

Si le répertoire existe, le code ci-dessus affichera "Le répertoire existe.", sinon il affichera "Le répertoire n'existe pas."

Il est également possible d'utiliser la fonction `is_dir()`, qui retourne `true` si le chemin spécifié correspond à un répertoire existant.

```PHP
if (is_dir("chemin/vers/repertoire")) {
    echo "Le chemin correspond à un répertoire existant.";
} else {
    echo "Le chemin ne correspond pas à un répertoire existant.";
}
```

Dans les deux cas, il est important de gérer les erreurs en utilisant des blocs `try-catch` pour éviter les plantages de script.

## Plongée en profondeur

Lorsque vous utilisez la fonction `file_exists()`, il est important de noter que le chemin du répertoire peut être spécifié à partir de deux positions différentes. Si le chemin est spécifié à partir de la racine du serveur, alors le répertoire sera recherché à partir de cette racine. Cependant, si le chemin est spécifié à partir du répertoire courant du script, alors le répertoire sera recherché à partir de ce répertoire.

De plus, il est possible de spécifier un chemin relatif ou absolu pour le répertoire. Un chemin relatif est relatif au répertoire courant du script tandis qu'un chemin absolu est un chemin complet à partir de la racine du serveur.

## Voir aussi

- [Documentation PHP sur `file_exists()`](https://www.php.net/manual/fr/function.file-exists.php)
- [Documentation PHP sur `is_dir()`](https://www.php.net/manual/fr/function.is-dir.php)
- [Article utile sur la gestion des erreurs en PHP](https://www.benmarshall.me/php-error-handling/)
- [Tutoriel sur les chemins relatifs et absolus en PHP](https://www.codementor.io/@michaelb/how-to-relative-absolute-paths-in-php-eg3e7q1kp)

Et voilà, vous savez maintenant comment vérifier si un répertoire existe en PHP ! N'hésitez pas à explorer plus en profondeur ces fonctions pour mieux les comprendre et les utiliser de manière appropriée dans vos projets.