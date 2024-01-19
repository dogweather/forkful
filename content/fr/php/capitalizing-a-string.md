---
title:                "Mettre en majuscule une chaîne"
html_title:           "PHP: Mettre en majuscule une chaîne"
simple_title:         "Mettre en majuscule une chaîne"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Capitaliser une chaîne de caractères, c'est-à-dire rendre la première lettre de chaque mot en majuscule. Les programmeurs font cela pour améliorer l'ergonomie et rendre le texte plus attrayant pour l'utilisateur.

## Comment faire :

Voyez l'exemple de code PHP simple ci-dessous qui utilise la fonction ucfirst():
```PHP
<?php
    $str = 'bonjour, monde!';
    echo ucfirst($str);
    // Outputs: Bonjour, monde!
?>
```
Si vous souhaitez capitaliser chaque mot dans la chaîne, utilisez la fonction ucwords():
```PHP
<?php
    $str = 'bonjour, monde!';
    echo ucwords($str);
    // Outputs: Bonjour, Monde!
?>
```

## Plongée plus profonde :

La capitalisation des chaînes était initialement une convention typographique en anglais utilisée pour mettre en évidence les noms propres et les débuts de phrases. Dans PHP, les fonctions ucfirst() et ucwords() sont disponibles depuis PHP 4 et restent les pistes les plus couramment utilisées pour la capitalisation des chaînes.

Il existe également d'autres méthodes, comme strtoupper(), qui convertit toute la chaîne en majuscules. En revanche, cette méthode n'est pas recommandée en raison de ses implications sur l'accessibilité.

## Voir aussi :

1. [Documentation PHP: ucwords()](https://www.php.net/manual/fr/function.ucwords.php)
2. [Documentation PHP: ucfirst()](https://www.php.net/manual/fr/function.ucfirst.php)
3. [Documentation PHP: strtoupper()](https://www.php.net/manual/fr/function.strtoupper.php)