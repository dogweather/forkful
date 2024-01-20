---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

La concaténation de chaînes est l'action de joindre deux ou plusieurs chaînes de caractères en une seule. Les programmeurs l'utilisent couramment pour manipuler des données textuelles, telles que générer des messages d'erreur dynamiques ou créer des requêtes SQL.

## Comment faire:

En PHP, l'opérateur de concaténation est un simple point `.`
Voici un exemple d'utilisation dans un bloc de code PHP :

```PHP
<?php
$phrase1 = "Bonjour, ";
$phrase2 = "monde!";
$phraseComplète = $phrase1 . $phrase2;
echo $phraseComplète;
?>
```

Et voilà ce que ça donne comme sortie :

```PHP
Bonjour, monde!
```

## Vue d'ensemble:

La concaténation de chaînes est un concept de programmation ancien et omniprésent, utilisé dans presque tous les langages. PHP offre l'alternative d'utiliser la fonction `sprintf` pour la concaténation, en particulier lors de la création des chaînes formatées. Cependant, l'utilisation de `.` pour concaténer des chaînes est plus facile à lire et à comprendre pour la plupart des développeurs.

PHP concatène physiquement les chaînes, ce qui peut être coûteux en termes de performance lorsque de grandes quantités de données sont manipulées. Cependant, dans la plupart des applications Web, cela n'a pas un impact notable sur les performances.

## Voir aussi:

Pour plus d'informations sur la concaténation de chaînes en PHP et les alternatives, consultez ces ressources :

- [PHP: L'opérateur de concaténation](https://www.php.net/manual/fr/language.operators.string.php)
- [PHP: `sprintf`](https://www.php.net/manual/fr/function.sprintf.php)