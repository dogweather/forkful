---
title:                "Interpoler une chaîne de caractères"
html_title:           "PHP: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que l'interpolation de chaîne et pourquoi les programmeurs le font-ils?

L'interpolation de chaîne en PHP est une technique qui permet de remplacer des variables ou des expressions à l'intérieur d'une chaîne de caractères par leur valeur réelle. Les programmeurs utilisent cette technique pour rendre leurs chaînes de caractères plus dynamiques et éviter d'écrire des valeurs fixes à chaque fois qu'ils veulent afficher une information différente.

## Comment faire:

Voici un exemple simple qui montre comment utiliser l'interpolation de chaîne en PHP:

```PHP
$name = "Jean";
echo "Bonjour $name, comment vas-tu?";
```
 
La sortie de ce code sera: "Bonjour Jean, comment vas-tu?"

On peut également utiliser des expressions plus complexes à l'intérieur d'une chaîne de caractères en utilisant les accolades:

```PHP
$age = 25;
echo "J'ai {$age} ans.";
```

La sortie de ce code sera: "J'ai 25 ans."

## Plongée en profondeur:

L'interpolation de chaîne a été introduite dans PHP 5.0 et est une alternative à la concaténation de chaînes avec le symbole '.'. Bien que la concaténation soit toujours largement utilisée, l'interpolation de chaîne se révèle plus pratique pour des chaînes plus longues et complexes. Il est également possible d'utiliser la fonction sprintf () pour effectuer une interpolation de chaîne, bien que cette méthode soit considérée comme plus lourde et moins lisible.

## Voir aussi:

- Les fonctions de chaînes de caractères en PHP: https://www.php.net/manual/fr/book.strings.php
- La documentation officielle de l'interpolation de chaîne en PHP: https://www.php.net/manual/fr/language.types.string.php#language.types.string.syntax.double