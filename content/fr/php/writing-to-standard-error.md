---
title:                "PHP: Ecrire sur l'erreur standard"
simple_title:         "Ecrire sur l'erreur standard"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi 

L'écriture sur la sortie d'erreur standard, ou "standard error", peut sembler une étape inutile dans un script PHP. Cependant, cela peut être très utile pour le débogage et la gestion des erreurs lors du développement d'applications web. Dans cet article, nous allons explorer pourquoi il est important d'écrire sur la sortie d'erreur standard et comment le faire efficacement. 

## Comment faire 

Pour écrire sur la sortie d'erreur standard en PHP, vous pouvez utiliser la fonction `fwrite()` en spécifiant le descripteur de fichier pour la sortie d'erreur standard, qui est `STDERR`. Par exemple : 

```PHP
fwrite(STDERR, "Erreur : Une variable n'a pas été définie");
```

Cela enverra un message d'erreur à la console ou au fichier de log, selon la configuration de votre serveur. Vous pouvez également utiliser `error_log()` pour écrire directement dans un fichier de log défini. 

Il est également important de noter que l'on peut utiliser `try...catch` pour capturer les erreurs et les écrire sur la sortie d'erreur standard. Par exemple : 

```PHP
try {
  // Code susceptible de générer une erreur
} catch (Exception $e) {
  fwrite(STDERR, "Erreur : ".$e->getMessage());
}
```

## Plongée en profondeur 

L'écriture sur la sortie d'erreur standard peut être particulièrement utile lors du développement d'applications web. Cela permet de détecter et de gérer les erreurs plus facilement, en les enregistrant dans un fichier dédié ou en les affichant dans la console pour un débogage rapide. 

En plus de cela, écrire sur la sortie d'erreur standard peut également être utilisé pour améliorer la sécurité de votre application. En logguant les erreurs, vous pouvez repérer et résoudre rapidement les vulnérabilités potentielles de votre code. 

## Voir aussi 

- [Tutoriel PHP complet pour les débutants](https://www.w3schools.com/php/)
- [Documentation officielle de PHP sur fwrite()](https://www.php.net/manual/fr/function.fwrite.php)
- [Guide de débogage PHP](https://www.php.net/manual/fr/debugger.php)