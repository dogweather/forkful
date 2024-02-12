---
title:                "Interpolation de chaînes de caractères"
aliases:
- fr/php/interpolating-a-string.md
date:                  2024-01-20T17:51:12.090083-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpoler une chaîne de caractères, c'est y insérer des variables directement. On le fait pour simplifier la concaténation et rendre le code plus lisible.

## How to:
```PHP
$planete = "Terre";
$message = "Bonjour, la $planete!";
echo $message; // Affiche : Bonjour, la Terre!

// Avec des accolades pour plus de clarté
$pommeCount = 3;
echo "J'ai {$pommeCount} pommes."; // Affiche : J'ai 3 pommes.
```

## Deep Dive
L'interpolation de chaînes a toujours fait partie de PHP. C'est rapide et facile, mais attention à ne pas l'utiliser avec des données non fiables pour éviter les injections. Alternative : `sprintf()` ou la concaténation avec `.`. Techniquement, l'interpolation ne se fait que dans les guillemets doubles et les heredocs.

## See Also
- [Documentation PHP sur les chaînes de caractères](https://www.php.net/manual/fr/language.types.string.php)
- [PHP: sprintf - Manual](https://www.php.net/manual/fr/function.sprintf.php)
