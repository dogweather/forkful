---
title:                "PHP: Transformer une chaîne de caractères en minuscules"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
Il est important de pouvoir convertir une chaîne de caractères en minuscules dans la programmation PHP, car cela permet de normaliser les données et facilite leur traitement ultérieur.

## Comment faire
Voici un exemple de code pour convertir une chaîne de caractères en minuscules en utilisant la fonction intégrée `strtolower` de PHP :

```PHP
$string = "CONVERT TO LOWERCASE";
$lowercase = strtolower($string);
echo $lowercase;
```

Cela devrait produire la sortie suivante :

```
convert to lowercase
```

## Plongée en profondeur
La fonction `strtolower` utilise l'algorithme de casse du système d'exploitation sur lequel elle est exécutée. Cela signifie qu'elle peut avoir un comportement différent en fonction de la langue ou du système d'exploitation utilisé. Par exemple, sur un système Windows, elle convertira également les caractères accentués en minuscules, tandis que sur un système Linux, ils resteront en majuscules.

De plus, il existe d'autres fonctions de casse en PHP, telles que `strtoupper` pour convertir en majuscules, `ucfirst` pour mettre la première lettre de chaque mot en majuscule et `ucwords` pour mettre en majuscule la première lettre de chaque mot d'une chaîne de caractères.

## Voir aussi
- [Documentation officielle de PHP sur la fonction strtolower](https://www.php.net/manual/fr/function.strtolower.php)
- [Article sur les différentes fonctions de casse en PHP](https://www.php.net/manual/fr/function.strtolower.php)
- [Github : exemples de conversion de casse de chaînes en PHP](https://github.com/php/php-src/blob/master/ext/standard/string.c)