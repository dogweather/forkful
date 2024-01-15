---
title:                "Convertir une chaîne en minuscules"
html_title:           "PHP: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous êtes peut-être déjà demandé pourquoi il est parfois nécessaire de convertir une chaîne de caractères en minuscules. Eh bien, cela peut être utile dans de nombreuses situations, notamment pour des fins de comparaison de chaînes ou pour une meilleure lisibilité du texte.

## Comment faire

Il existe une fonction native en PHP appelée `strtolower()` qui permet de convertir une chaîne en minuscules. Voici un example de code pour l'utiliser :

```PHP
$string = "VOICI UNE CHAÎNE DE CARACTÈRES";
echo strtolower($string);
```

Cela produira la sortie suivante :
```PHP
voici une chaîne de caractères
```

Il est également possible d'utiliser cette fonction sur un tableau de chaînes, en utilisant la fonction `array_map()`. Par exemple :

```PHP
$strings = ["VOICI", "UNE", "CHAÎNE", "DE", "CARACTÈRES"];
$lowercase_strings = array_map("strtolower", $strings);
print_r($lowercase_strings);
```

Cela produira la sortie suivante :
```PHP
Array
(
    [0] => voici
    [1] => une
    [2] => chaîne
    [3] => de
    [4] => caractères
)
```

## Plongez plus profondément

En fait, cette fonction utilise la norme Unicode pour la conversion en minuscules, ce qui signifie qu'elle prend en compte les caractères dans toutes les langues. Cela peut être particulièrement utile si vous travaillez avec du texte multilingue.

Cependant, il est important de noter que la conversion en minuscules ne se fait pas toujours de manière simple. Par exemple, la lettre "I" majuscule en anglais devient "i" minuscule en français, mais en allemand elle reste "I" avec un tréma sur le dessus. Cela peut poser des problèmes si vous effectuez des opérations de comparaison de chaînes dans différentes langues.

## Voir aussi

- [Documentation officielle de la fonction strtolower() en PHP](https://www.php.net/manual/fr/function.strtolower.php)
- [Différentes méthodes pour convertir une chaîne en minuscules en PHP](https://www.w3schools.com/php/php_string_lowercase.asp)
- [Explications détaillées sur la norme Unicode pour la conversion en minuscules](https://en.wikipedia.org/wiki/Unicode_character_property#Case_folding)