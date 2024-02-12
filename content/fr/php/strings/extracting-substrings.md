---
title:                "Extraction de sous-chaînes"
aliases:
- fr/php/extracting-substrings.md
date:                  2024-01-20T17:46:13.414518-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extraire des sous-chaînes, c'est comme sculpter un petit bout de bois d'un gros tronc; en PHP, cela signifie prendre une partie d'une chaîne de caractères. Les développeurs font ça pour analyser des données, valider des entrées ou simplement afficher quelque chose de spécifique à un utilisateur.

## How to:
PHP utilise la fonction `substr()` pour extraire des sous-chaînes. Simple, droit au but. Voici comment ça fonctionne :

```php
$texte = "Bonjour, je suis un script PHP!";

// Pour obtenir "Bonjour"
echo substr($texte, 0, 7);  // Affiche: Bonjour

// Pour extraire "script"
echo substr($texte, 17, 6); // Affiche: script

// En partant de la fin, obtenir "PHP"
echo substr($texte, -4);    // Affiche: PHP!
```

## Deep Dive
`substr()` a des racines qui remontent à la programmation C - c'est un classique. Mais PHP a aussi `mb_substr()`, essentiel pour les chaînes multioctets, genre UTF-8. Pourquoi? Parce que `substr()` peut couper en plein milieu d'un caractère multioctet, ce qui brise tout. La fonction `strstr()` et `strpos()` sont d'autres alternatives, utiles pour certains cas spécifiques.

Dans le ventre de PHP, `substr()` est implémenté en C et opère directement sur le stockage binaire de la chaîne. C'est pour ça que dans des langues à caractères complexes, cela peut mener à des comportements inattendus sans `mbstring`.

## See Also
- [`substr()` dans le manuel PHP](https://www.php.net/manual/fr/function.substr.php)
- [Manuel PHP sur les chaînes de caractères](https://www.php.net/manual/fr/book.strings.php)
- [Documentation `mb_substr()` pour travailler avec l'UTF-8](https://www.php.net/manual/fr/function.mb-substr.php)
