---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi ? 

La recherche de la longueur d'une chaîne consiste à déterminer le nombre de caractères dans une chaîne donnée. Les programmeurs le font pour valider les entrées, tronquer les textes, comparer des chaînes, et plus encore.

## Comment faire:

En PHP, nous utilisons la fonction `strlen()` pour trouver la longueur d'une chaîne. Voici comment ça fonctionne :

```PHP
<?php
$texte = "Bonjour le monde!";
echo strlen($texte);
?>
```

Cela générera :

```PHP
17
```
Vous obtiendrez le nombre de caractères, y compris les espaces, dans la chaîne.

## Plongée profonde:

Historiquement, `strlen()` existe depuis la première version de PHP, lorsque PHP était principalement utilisé pour le traitement des chaînes. 

Il existe également des alternatives à `strlen()`. Par exemple, `mb_strlen()` est utilisé lorsque vous travaillez avec des chaînes multibyte, c'est à dire qui peuvent contenir des caractères non-latin, comme les kanjis japonais.

En termes de détails de mise en œuvre, `strlen()` compte le nombre d'octets plutôt que le nombre de caractères. Cela signifie que pour les caractères multi-octets, kao les emojis, la fonction peut retourner un résultat inattendu.

## Voir également:

Pour plus d'informations sur les chaînes en PHP, consultez les pages suivantes :
- [PHP strlen()](https://www.php.net/manual/fr/function.strlen.php)
- [PHP mb_strlen()](https://www.php.net/manual/fr/function.mb-strlen.php)
- [Les Chaînes de caractères en PHP](https://www.php.net/manual/fr/language.types.string.php)