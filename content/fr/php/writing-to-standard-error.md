---
title:    "PHP: Écrire vers l'erreur standard"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

L'écriture vers l'erreur standard (standard error) est une pratique courante en programmation PHP. Elle permet de signaler des erreurs, des avertissements ou des informations importantes lors de l'exécution d'un programme. Les messages d'erreur peuvent être utiles pour le débogage et l'amélioration de la qualité du code.

## Comment faire

Pour écrire vers l'erreur standard en PHP, il suffit d'utiliser la fonction standard **error_log()**. Cette fonction prend en paramètre le message à afficher et optionnellement le code d'erreur et le chemin vers le fichier de sortie. Voici un exemple de son utilisation :

```PHP
<?php
// Ecrire un message d'erreur
$message = "Une erreur s'est produite dans le code.";
error_log($message);
// Ecrire un message d'erreur avec un code spécifique
$code = 404;
error_log($message, $code);
// Ecrire un message d'erreur dans un fichier spécifique
$file = "logs.txt";
error_log($message, 3, $file);
```

Le code ci-dessus écrira le message d'erreur dans le fichier de sortie par défaut, qui est généralement **stderr**. Si vous souhaitez écrire directement dans le fichier de sortie, vous pouvez utiliser la constante **STDERR** avec la fonction **fwrite()**.

```PHP
<?php
// Ecrire un message d'erreur dans le fichier de sortie
$file = fopen('php://stderr', 'w');
fwrite($file, "Un autre message d'erreur.");
fclose($file);
```

## Plongée en profondeur

L'erreur standard est un canal de communication important entre un programme et l'utilisateur. En plus des messages d'erreur, elle peut également être utilisée pour afficher des informations de débogage ou des avertissements. Il est important de comprendre comment utiliser correctement l'erreur standard pour éviter toute confusion dans le code.

Il est également possible de rediriger la sortie d'erreur vers un fichier spécifique, en utilisant la fonction **ini_set()** avec l'option **error_log**. Cela peut être utile lors du débogage d'une application en production sans perturber les utilisateurs.

## Voir aussi

- [Documentation PHP sur la fonction error_log()](https://www.php.net/manual/fr/function.error-log.php) 
- [Tutoriel sur la gestion des erreurs en PHP](https://www.php.net/manual/fr/language.exceptions.php)
- [Article sur la redirection de la sortie d'erreur en PHP](https://www.php.net/manual/fr/errorfunc.configuration.php#ini.error-log)