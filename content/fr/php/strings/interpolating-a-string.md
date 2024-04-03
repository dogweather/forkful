---
date: 2024-01-20 17:51:12.090083-07:00
description: "Interpoler une cha\xEEne de caract\xE8res, c'est y ins\xE9rer des variables\
  \ directement. On le fait pour simplifier la concat\xE9nation et rendre le code\
  \ plus lisible."
lastmod: '2024-03-13T22:44:57.863603-06:00'
model: gpt-4-1106-preview
summary: "Interpoler une cha\xEEne de caract\xE8res, c'est y ins\xE9rer des variables\
  \ directement."
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

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
