---
title:                "Extraction de sous-chaînes"
html_title:           "PHP: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
L'extraction de sous-chaînes de caractères est une tâche courante pour les programmeurs PHP. Cela consiste à extraire un morceau spécifique d'une chaîne de caractères, en utilisant des positions de début et de fin déterminées. Les programmeurs utilisent souvent cette fonctionnalité pour manipuler et traiter des données textuelles de manière plus efficace.

## Comment faire:
Voici deux exemples de code qui utilisent la fonction `substr()` pour extraire des sous-chaînes de caractères en PHP.

```PHP
// Exemple 1: Extraire les 3 premiers caractères d'une chaîne
$string = "Bonjour";
$first_three_letters = substr($string, 0, 3);
// output: Bon

// Exemple 2: Extraire les 5 derniers caractères d'une chaîne
$string = "Hello World!";
$last_five_letters = substr($string, -5);
// output: World!
```

## Plongée en profondeur:
L'extraction de sous-chaînes de caractères est un concept qui existe depuis longtemps. Dans les versions antérieures de PHP, la fonction `substr()` était la seule option pour extraire des sous-chaînes. Cependant, avec l'introduction de la fonction `mb_substr()` dans PHP 4.0, les programmeurs peuvent maintenant extraire des sous-chaînes de caractères multibyte, ce qui était auparavant une tâche plus difficile.

Il existe également d'autres alternatives à la fonction `substr()`, telles que `str_split()`, qui peut diviser une chaîne en un tableau de caractères, ou `preg_match()`, qui utilise des expressions régulières pour extraire des sous-chaînes qui correspondent à un modèle spécifique.

En ce qui concerne les détails d'implémentation, il est important de noter que la fonction `substr()` retourne une valeur de chaîne vide si le point de départ demandé est égal à la longueur de la chaîne d'origine. De plus, lorsque la longueur demandée est un nombre négatif, la fonction retourne une chaîne vide si la valeur absolue du nombre est supérieure à la longueur de la chaîne d'origine.

## Voir aussi:
- Documentation officielle de PHP sur la fonction `substr()`: https://www.php.net/manual/fr/function.substr.php
- Tutoriel sur l'utilisation des expressions régulières en PHP: https://www.w3schools.com/php/php_regex.asp
- Comparaison entre les fonctions `substr()` et `mb_substr()`: https://stackoverflow.com/questions/2327818/what-is-the-difference-between-substr-and-mb-substr-in-php