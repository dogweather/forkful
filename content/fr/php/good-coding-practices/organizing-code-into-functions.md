---
date: 2024-01-26 01:11:12.895833-07:00
description: "Organiser le code en fonctions consiste \xE0 d\xE9composer votre code\
  \ en blocs r\xE9utilisables ayant des objectifs d\xE9finis. Nous le faisons pour\
  \ maintenir une\u2026"
lastmod: '2024-03-13T22:44:57.884560-06:00'
model: gpt-4-1106-preview
summary: "Organiser le code en fonctions consiste \xE0 d\xE9composer votre code en\
  \ blocs r\xE9utilisables ayant des objectifs d\xE9finis. Nous le faisons pour maintenir\
  \ une\u2026"
title: Organisation du code en fonctions
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Organiser le code en fonctions consiste à décomposer votre code en blocs réutilisables ayant des objectifs définis. Nous le faisons pour maintenir une certaine clarté, prévenir la redondance et simplifier le débogage.

## Comment faire :
Imaginez que nous ayons du code répétitif pour saluer les utilisateurs. À la place, nous allons l'encapsuler dans une fonction comme `greet_user` :

```php
function greet_user($name) {
    return "Bonjour, " . $name . " !";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Sortie :
```
Bonjour, Alice !
Bonjour, Bob !
```

Maintenant, vous avez un outil pratique que vous pouvez utiliser à tout moment sans réécrire les mêmes lignes de code chaque fois que vous souhaitez dire bonjour.

## Exploration détaillée
Les fonctions sont présentes dans la programmation depuis les premiers jours du FORTRAN dans les années 50. Elles sont une pierre angulaire de la programmation structurée et mettent l'accent sur la modularité et l'isolation. Des alternatives ? Eh bien, vous pouvez adopter la programmation orientée objet et parler de classes et méthodes, qui sont des fonctions avec un costume élégant. Concernant PHP, les détails de mise en œuvre incluent la spécification de valeurs par défaut pour les paramètres, la déclaration de type pour les entrées et la possibilité de retourner plusieurs valeurs en utilisant un tableau ou, à partir de PHP 7.1, une liste.

Voici une touche moderne avec déclaration de type et valeurs par défaut :

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 a également introduit les fonctions fléchées, aidant à écrire des fonctions concises sur une seule ligne, couramment utilisées dans les opérations sur les tableaux :

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Sortie :
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Voir Aussi
- [Manuel PHP sur les Fonctions](https://www.php.net/manual/fr/functions.user-defined.php)
- [PHP : La Bonne Manière - Fonctions](https://phptherightway.com/#functions)
- [Apprendre sur les Fonctions fléchées de PHP 7.4](https://stitcher.io/blog/short-closures-in-php)
