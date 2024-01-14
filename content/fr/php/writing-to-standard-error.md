---
title:                "PHP: Écrire vers les erreurs standards"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi quelqu'un utiliserait standard error lors de l'écriture de code PHP ? La réponse est simple : il y a des situations où la sortie standard (standard output) n'est pas suffisante pour fournir des informations sur les erreurs. C'est là que l'écriture à standard error peut être utile. Cela permet de séparer les messages d'erreur et les messages de débogage, rendant la lecture des informations de débogage plus facile et plus efficace.

## Comment faire

Pour écrire à standard error en PHP, vous pouvez utiliser la fonction `fwrite()` en spécifiant une sortie de type `STDERR`. Voici un exemple de code :

```PHP
<?php
$vers = 5;
$division = 10 / $vers;

if ($division == 2.5) {
    echo "La division s'est effectuée correctement.\n";
} else {
    fwrite(STDERR, "Il y a eu une erreur lors de la division.\n");
}
```

Lorsque vous exécutez ce code, le message d'erreur sera affiché dans le terminal plutôt que dans l'affichage standard. Cela peut s'avérer très utile lors du débogage et du test de votre code.

## Plongée en profondeur

Il est important de noter que l'écriture à standard error peut également être utilisée pour enregistrer des messages d'erreur dans un fichier de journal au lieu de les afficher directement dans le terminal. Cela peut être utile lors du développement de grandes applications qui nécessitent une surveillance et une gestion des erreurs plus étendues.

Une autre astuce utile consiste à utiliser la fonction `error_reporting()` pour contrôler le niveau de rapport d'erreurs et ainsi avoir un meilleur contrôle sur les messages d'erreur qui seront écrits à standard error.

## Voir aussi

Pour en savoir plus sur l'utilisation de standard error en PHP, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de PHP sur la fonction `fwrite()`](https://www.php.net/manual/fr/function.fwrite.php)
- [Article sur le site Mastering PHP sur la gestion des erreurs avec PHP](https://masteringphp.net/exception-handling-with-try-catch-finally/)
- [Discussion sur Stack Overflow sur la différence entre les sorties standard et les sorties d'erreur en PHP](https://stackoverflow.com/questions/37803660/difference-between-php-echo-and-fwrite-stderr)

N'hésitez pas à explorer et à expérimenter avec l'écriture à standard error dans vos projets PHP pour améliorer votre processus de débogage et de gestion des erreurs. Bonne programmation !