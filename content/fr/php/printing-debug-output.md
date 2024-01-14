---
title:                "PHP: Afficher la sortie de débogage"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il est souvent nécessaire de trouver des erreurs dans notre code. Cela peut être frustrant et prendre beaucoup de temps, mais il y a un outil simple qui peut grandement faciliter ce processus : le débogage. En affichant des messages de débogage, nous pouvons comprendre comment notre code s'exécute et trouver plus facilement les erreurs. Dans cet article, nous allons explorer comment utiliser le débogage en PHP pour améliorer notre expérience de programmation.

## Comment faire

Pour afficher des messages de débogage en PHP, nous pouvons utiliser la fonction `echo` pour imprimer du texte dans notre code. Par exemple :

```PHP
<?php
    $variable = "Bonjour";
    echo $variable;
?>
```

Cela affichera "Bonjour" dans le navigateur lorsque notre code s'exécutera. Mais que se passe-t-il si nous voulons voir la valeur d'une variable pour comprendre pourquoi notre code ne fonctionne pas correctement ? Nous pouvons utiliser la fonction `var_dump` pour obtenir une description détaillée de la variable. Par exemple :

```PHP
<?php
    $age = 25;
    var_dump($age);
?>
```

Cela affichera `int(25)` dans le navigateur, nous indiquant que la variable `$age` est un entier avec la valeur 25. Nous pouvons également utiliser la fonction `print_r` pour obtenir une représentation lisible de la variable. Par exemple :

```PHP
<?php
    $pays = array("France", "Espagne", "Italie");
    print_r($pays);
?>
```

Cela affichera `Array ( [0] => France [1] => Espagne [2] => Italie )` dans le navigateur, nous montrant le contenu de notre tableau.

## Plongée en profondeur

Bien que l'affichage de messages de débogage puisse sembler simple, il existe en réalité de nombreuses façons d'afficher des informations utiles pour comprendre notre code. Par exemple, nous pouvons utiliser la fonction `error_log` pour écrire des messages de débogage dans un fichier plutôt que de les afficher dans le navigateur. De plus, nous pouvons définir des niveaux de débogage pour n'afficher que certaines informations en fonction de nos besoins. Nous pouvons également utiliser des outils tels que Xdebug pour un débogage plus avancé.

## Voir aussi

- [Documentation officielle de PHP sur le débogage](https://www.php.net/manual/fr/function.var-dump.php)
- [Guide du débogage en PHP sur Codecademy](https://www.codecademy.com/fr/courses/learn-debugging-with-php)
- [Utiliser Xdebug pour le débogage en PHP](https://www.sitepoint.com/debug-php-xdebug/)