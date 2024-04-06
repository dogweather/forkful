---
date: 2024-01-20 17:51:12.090083-07:00
description: "How to: L'interpolation de cha\xEEnes a toujours fait partie de PHP.\
  \ C'est rapide et facile, mais attention \xE0 ne pas l'utiliser avec des donn\xE9\
  es non fiables\u2026"
lastmod: '2024-04-05T21:53:59.348804-06:00'
model: gpt-4-1106-preview
summary: "L'interpolation de cha\xEEnes a toujours fait partie de PHP."
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

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
