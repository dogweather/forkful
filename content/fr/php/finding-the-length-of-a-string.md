---
title:                "PHP: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous pouvez vous demander pourquoi trouver la longueur d'une chaîne de caractères est important en programmation PHP. La longueur d'une chaîne peut être utile pour de nombreuses tâches, comme la validation de données, la manipulation de texte et la génération de rapports.

## Comment faire

Pour trouver la longueur d'une chaîne en PHP, vous pouvez utiliser la fonction `strlen()`. Cette fonction prend en paramètre une chaîne de caractères et renvoie le nombre de caractères qu'elle contient. Voici un exemple de code :

```PHP
$string = "Bonjour le monde!";
echo strlen($string);
```

Lorsque vous exécutez ce code, vous verrez `18` s'afficher, car la chaîne contient 18 caractères. Vous pouvez également utiliser cette fonction pour vérifier si une chaîne a une longueur spécifique, en la comparant à une valeur donnée. Par exemple :

```PHP
if (strlen($string) > 10) {
  echo "Cette chaîne est plus longue que 10 caractères!";
} else {
  echo "Cette chaîne est plus courte que 10 caractères!";
}
```

Lorsque vous exécutez ce code, vous verrez `Cette chaîne est plus longue que 10 caractères!` s'afficher, car la chaîne a une longueur de 18 caractères.

## Plongée en profondeur

Il est important de comprendre que la fonction `strlen()` compte le nombre de caractères dans une chaîne, et non le nombre de mots. Ainsi, si votre chaîne contient des espaces, ceux-ci seront également pris en compte dans la longueur totale. De plus, il est important de noter que cette fonction ne prend pas en compte les accents ou les caractères spéciaux, donc si vous avez besoin de compter ces caractères, il faudra utiliser une autre méthode.

## Voir aussi

- [Documentation de la fonction `strlen()` en PHP](https://www.php.net/manual/fr/function.strlen.php)
- [Exemples de manipulation de chaînes en PHP](https://www.developpez.net/forums/d1867283/php/langage/php/manipuler-chaine-caracteres-exemples/)