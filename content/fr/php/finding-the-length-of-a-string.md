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

## Pourquoi

La recherche de la longueur d'une chaîne de caractères est une opération courante en programmation, car cela permet de manipuler des données spécifiques à chaque caractère d'une chaîne. Cela peut également être utile pour vérifier si une chaîne dépasse une certaine limite de longueur.

## Comment faire

Pour trouver la longueur d'une chaîne en PHP, vous pouvez utiliser la fonction prédéfinie `strlen()`. Cette fonction prend une chaîne en tant que paramètre et renvoie le nombre de caractères présents dans cette chaîne.

Exemple de code :
```PHP
$str = "Bonjour tout le monde!";
echo "La longueur de la chaîne est : " . strlen($str) . " caractères.";
```
Résultat :
```
La longueur de la chaîne est : 21 caractères.
```

Il est également possible d'utiliser la méthode `mb_strlen()` pour calculer la longueur d'une chaîne prenant en compte les caractères multibytes.

## Plongeon en profondeur

La fonction `strlen()` utilise en fait l'encodage des chaînes de caractères pour compter le nombre de caractères. Cela signifie que, pour les chaînes avec des caractères multibytes, la longueur peut être différente selon l'encodage choisi.

De plus, la fonction `strlen()` ne comptera pas les espaces ou les autres caractères spéciaux comme des caractères. Pour prendre en compte ces caractères dans le calcul de la longueur d'une chaîne, vous pouvez utiliser `mb_strlen()` avec l'option `mb_strlen($str, 'UTF-8')`.

## Voir aussi

- [Documentation officielle de la fonction `strlen()` en PHP](https://www.php.net/manual/fr/function.strlen.php)
- [Documentation officielle de la fonction `mb_strlen()` en PHP](https://www.php.net/manual/fr/function.mb-strlen.php)
- [Un guide complet sur la manipulation de chaînes en PHP](https://www.phptherightway.com/#strings)