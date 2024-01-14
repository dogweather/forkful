---
title:    "PHP: Créer un fichier temporaire"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en PHP?

Il y a plusieurs raisons pour lesquelles vous pourriez avoir besoin de créer un fichier temporaire lors de la programmation en PHP. Par exemple, cela peut être utile si vous devez stocker des données temporaires ou si vous souhaitez créer un système de cache pour améliorer les performances de votre application.

## Comment créer un fichier temporaire en PHP?

Il existe plusieurs façons de créer un fichier temporaire en utilisant PHP, mais l'une des méthodes les plus simples est d'utiliser la fonction `tmpfile()`. Voici un exemple de code pour créer et écrire dans un fichier temporaire:

```PHP
$fichier_temp = tmpfile();
if ($fichier_temp) {
   fwrite($fichier_temp, "Ceci est un exemple de données écrites dans le fichier temporaire.");
   // Vous pouvez faire d'autres opérations sur le fichier ici
   fclose($fichier_temp); // permet de supprimer le fichier temporaire après utilisation
}
```

Lors de l'exécution de ce code, un nouveau fichier temporaire sera créé avec un nom et un emplacement aléatoires. Vous pouvez également spécifier un nom et un emplacement personnalisés si vous le souhaitez, en utilisant la fonction `tmpnam()`.

## Plongée en profondeur dans la création de fichiers temporaires

Lorsque vous utilisez la fonction `tmpfile()`, le fichier temporaire sera automatiquement supprimé lorsque votre script se termine ou lorsque vous utilisez la fonction `fclose()` pour le fermer. Cependant, si vous utilisez la fonction `tmpnam()`, le fichier temporaire ne sera pas automatiquement supprimé, il est donc de votre responsabilité de le faire.

Vous pouvez également utiliser la fonction `sys_get_temp_dir()` pour obtenir le chemin du répertoire temporaire par défaut de votre système d'exploitation, ce qui peut être utile si vous souhaitez stocker vos fichiers temporaires dans un endroit spécifique.

## Voir aussi

- [Documentation officielle de PHP sur les fichiers temporaires](https://www.php.net/manual/en/function.tmpfile.php)
- [Article sur les performances de l'utilisation des fichiers temporaires en PHP](https://www.phparch.com/2019/06/php-benchmark-series-part-3-files-and-databases/)
- [Une alternative à la fonction `tmpfile()` en utilisant des fichiers mémoire](https://thisinterestsme.com/php-creating-temporary-files-memory/)