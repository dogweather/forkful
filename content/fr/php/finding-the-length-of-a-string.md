---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "PHP: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?

Trouver la longueur d'une chaîne de caractères est une tâche courante pour les programmeurs PHP. Cela consiste simplement à déterminer le nombre de caractères composant une chaîne de texte. Cette information est souvent utile pour effectuer des opérations de manipulation de chaînes, telles que la vérification de la validité d'un mot de passe ou la limitation du nombre de caractères dans un champ de saisie.

## Comment faire :

```PHP
<?php
// Utilisation de la fonction strlen() pour trouver la longueur d'une chaîne
$chaine = "Bonjour le monde";
echo strlen($chaine); // Affiche 16
?>
```

```PHP
<?php
// Utilisation de la fonction mb_strlen() pour trouver la longueur d'une chaîne multibyte
$texte = "こんにちは世界"; // Chaîne de caractères en japonais
echo mb_strlen($texte); // Affiche 6
?>
```

## Plongée en profondeur :

La fonction strlen() a été introduite dès la première version de PHP en 1995. Elle a depuis été améliorée pour mieux gérer les caractères multibyte, qui occupent plus d'un octet. Pour cela, la fonction mb_strlen() a été créée. Elle est basée sur la bibliothèque d'extension mbstring et peut gérer efficacement les caractères non-ASCII.

Cependant, pour les chaînes de caractères très longues, notamment celles provenant de bases de données, utiliser strlen() ou mb_strlen() peut ralentir les performances de votre script. Dans ce cas, il peut être plus efficace d'utiliser la fonction PHP native strcspn() qui permet de trouver la longueur d'une chaîne en utilisant une liste de caractères à ignorer.

## Voir aussi :

- La documentation sur strlen() : https://www.php.net/manual/fr/function.strlen.php
- La documentation sur mb_strlen() : https://www.php.net/manual/fr/function.mb-strlen.php
- La documentation sur strcspn() : https://www.php.net/manual/fr/function.strcspn.php