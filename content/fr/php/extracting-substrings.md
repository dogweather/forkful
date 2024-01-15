---
title:                "Extraction de sous-chaînes"
html_title:           "PHP: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être rencontré des situations où vous aviez besoin d'extraire une partie spécifique d'une chaîne de caractères. Cela pourrait être des numéros de téléphone, des adresses email, ou même des données de grande taille à analyser. Dans ces cas, il est utile de savoir comment extraire facilement des sous-chaînes à partir d'une chaîne principale en utilisant PHP.

## Comment faire

Tout d'abord, pour extraire une sous-chaîne à partir d'une chaîne principale, nous allons utiliser la fonction PHP `substr()`. Cette fonction prend deux arguments obligatoires et un argument facultatif.

Voici un exemple de code PHP montrant comment utiliser la fonction `substr()` pour extraire une sous-chaîne:

```
$string = "Bienvenue sur notre site";
$start = 12;
$length = 6;
$subString = substr($string, $start, $length);
echo $subString;
```

Dans cet exemple, nous avons défini une chaîne de caractères et décidé de commencer à extraire une sous-chaîne à partir du 12ème caractère jusqu'au 6ème caractère suivant. Le résultat de cette opération serait "notre".

En utilisant les arguments facultatifs, nous pouvons également extraire une sous-chaîne à partir d'une position donnée jusqu'à la fin de la chaîne principale, ou même une sous-chaîne à partir d'une position négative en commençant de la fin de la chaîne principale.

## Deep Dive

Si vous devez extraire des sous-chaînes régulièrement dans votre code, il pourrait être utile de connaître d'autres fonctions qui peuvent également effectuer cette tâche.

Par exemple, la fonction `strstr()` peut être utilisée pour trouver la première occurrence d'une sous-chaîne dans une chaîne principale et extraire toutes les autres caractères à partir de cette position. La fonction `str_replace()` peut également être utilisée pour remplacer une sous-chaîne par une autre dans une chaîne principale.

Il est également important de noter que la position des caractères dans une chaîne de caractères commence à zéro, ce qui signifie que le premier caractère est en position 0, le deuxième en position 1, et ainsi de suite.

## Voir aussi

Pour plus d'informations sur les fonctions de manipulation de chaînes de caractères en PHP, vous pouvez consulter la documentation officielle de PHP sur les fonctions de chaînes de caractères.

- [Documentation PHP sur la fonction `substr()`](https://www.php.net/manual/fr/function.substr.php)
- [Documentation PHP sur la fonction `strstr()`](https://www.php.net/manual/fr/function.strstr.php)
- [Documentation PHP sur la fonction `str_replace()`](https://www.php.net/manual/fr/function.str-replace.php)