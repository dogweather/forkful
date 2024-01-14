---
title:                "PHP: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi 
Saviez-vous que la vérification de l'existence d'un répertoire est une pratique courante dans de nombreux projets de programmation PHP ? C'est une étape importante pour s'assurer que les fichiers sont accessibles et organisés de manière efficace. Dans cet article, nous allons vous guider à travers le processus de vérification d'un répertoire existant en utilisant PHP.

# Comment faire
Pour vérifier si un répertoire existe en utilisant PHP, nous utiliserons la fonction `is_dir()`. Cette fonction prend un paramètre : le chemin du répertoire à vérifier. Voyons un exemple :

```PHP
$directory = "/var/www/html/images";
if (is_dir($directory)) {
    echo "Le répertoire $directory existe.";
} else {
    echo "Le répertoire $directory n'existe pas.";
}
```

Dans cet exemple, nous vérifions si le répertoire `/var/www/html/images/` existe en utilisant `is_dir()`. Si c'est le cas, le message "Le répertoire existe" sera affiché, sinon le message "Le répertoire n'existe pas" sera affiché.

# Plongée profonde
La fonction `is_dir()` fonctionne en vérifiant si le chemin fourni est un répertoire valide. Elle renvoie `true` si c'est le cas, sinon elle renvoie `false`. Il est important de noter que cette fonction ne vérifie pas si le répertoire est vide ou non.

Une autre fonction utile pour la vérification d'un répertoire est `opendir()`. Cette fonction ouvre un répertoire spécifié et retourne un pointeur de dossier pour être utilisé par d'autres fonctions telles que `readdir()` et `rewinddir()`. Voyons un exemple :

```PHP
$directory = "/var/www/html/images";
$dir_handle = opendir($directory);
if ($dir_handle) {
    echo "Le répertoire $directory a été ouvert avec succès.";
    closedir($dir_handle);
} else {
    echo "Impossible d'ouvrir le répertoire $directory.";
}
```

Dans cet exemple, nous appelons `opendir()` pour ouvrir le répertoire `/var/www/html/images`, puis vérifions si le pointeur de dossier a été correctement retourné. Nous utilisons ensuite la fonction `closedir()` pour fermer le pointeur de dossier une fois que nous avons terminé de l'utiliser.

# Voir aussi
- [Documentation PHP pour`is_dir()`](https://www.php.net/manual/en/function.is-dir.php)
- [Documentation PHP pour`opendir()`](https://www.php.net/manual/en/function.opendir.php)
- [Tutoriel sur la gestion des fichiers en PHP](https://www.tutorialspoint.com/php/php_files_management.htm)