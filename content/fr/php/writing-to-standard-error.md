---
title:    "PHP: Écrire sur l'erreur standard"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi
Écrire dans la sortie d'erreur standard peut sembler intimidant au premier abord, mais c'est un outil essentiel pour les développeurs PHP. En écrivant dans la sortie d'erreur standard, vous pouvez facilement suivre et déboguer vos scripts, car les messages d'erreur seront affichés directement dans votre console ou votre serveur.

## Comment faire
Pour écrire dans la sortie d'erreur standard en PHP, utilisez la fonction `fwrite()` en spécifiant `STDERR` comme argument pour indiquer que vous voulez écrire dans la sortie d'erreur standard. Voici un exemple de code pour écrire une erreur dans la sortie d'erreur standard :

```PHP
fwrite(STDERR, "Une erreur s'est produite - fichier introuvable");
```

Lorsque vous exécutez ce code, vous obtiendrez le message d'erreur dans votre console ou votre serveur.

## Profondeur
Écrire dans la sortie d'erreur standard peut sembler simple, mais il y a quelques points à garder à l'esprit. Tout d'abord, assurez-vous que votre code est bien encapsulé dans une condition `if()` pour éviter qu'il ne s'exécute en cas de suppression de l'affichage des erreurs. De plus, il est recommandé d'utiliser la fonction `error_log()` plutôt que `fwrite()` pour écrire dans la sortie d'erreur standard, car cela vous permettra de spécifier un fichier de log spécifique pour les erreurs.

## Voir aussi
Voici quelques liens utiles pour en savoir plus sur l'écriture dans la sortie d'erreur standard en PHP :

- [Documentation officielle sur la fonction fwrite](https://www.php.net/manual/fr/function.fwrite.php)
- [Tutoriel vidéo sur l'utilisation de la sortie d'erreur standard en PHP](https://www.youtube.com/watch?v=67DlKzHbbmE)
- [Article de blog sur les bonnes pratiques pour écrire dans la sortie d'erreur standard](https://blog.servergrove.com/2019/04/30/capture-php-errors-and-write-to-standard-error-output/)