---
date: 2024-01-20 17:35:08.271994-07:00
description: "How to (Comment faire) Fusionner des cha\xEEnes en PHP est un jeu d'enfant.\
  \ Utilisez le point `.` pour les coller ensemble. Voil\xE0 un exemple ."
lastmod: '2024-04-05T21:53:59.355177-06:00'
model: gpt-4-1106-preview
summary: Utilisez le point `.` pour les coller ensemble.
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## How to (Comment faire)
Fusionner des chaînes en PHP est un jeu d'enfant. Utilisez le point `.` pour les coller ensemble. Voilà un exemple :

```PHP
<?php
$greeting = "Bonjour";
$name = "Marie";
$message = $greeting . ", " . $name . " !";

echo $message; // Affiche : Bonjour, Marie !
?>
```

Vous pouvez aussi concaténer et affecter en même temps avec `.=` :

```PHP
<?php
$message = "Bonne";
$message .= " chance"; // $message vaut maintenant "Bonne chance"

echo $message; // Affiche : Bonne chance
?>
```

## Deep Dive (Plongée en Profondeur)
Historiquement, la concaténation de chaînes est aussi ancienne que les premiers langages de programmation. En PHP, le point est le symbole de prédilection depuis sa création dans les années 90. 

Alternativement, depuis PHP 5.3.0, la fonction `implode()` est particulièrement utile pour joindre les éléments d'un tableau :

```PHP
<?php
$array = ["PHP", "c'est", "super"];
echo implode(" ", $array); // Affiche : PHP c'est super
?>
```

Concernant les détails d'implémentation, chaque concaténation peut allouer de la mémoire, donc attention à la performance si vous travaillez avec des textes très longs. Heureusement, PHP est généralement assez efficace pour gérer cela.

## See Also (Voir Aussi)
- La documentation officielle de PHP sur les chaînes de caractères : [php.net](https://www.php.net/manual/fr/language.types.string.php)
- Un guide sur la performance et l'optimisation des chaînes en PHP : [PHP: The Right Way](https://phptherightway.com/#performance)
