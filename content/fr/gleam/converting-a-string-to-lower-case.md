---
title:                "Conversion d'une chaîne de caractères en minuscules"
html_title:           "Gleam: Conversion d'une chaîne de caractères en minuscules"
simple_title:         "Conversion d'une chaîne de caractères en minuscules"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & pourquoi ?

La conversion d'une chaîne de caractères en minuscules est une opération courante dans la programmation. Cela signifie simplement changer tous les caractères en majuscules d'une chaîne en leurs équivalents en minuscules. Les programmeurs le font généralement pour uniformiser les données et rendre la recherche et le tri plus faciles.

## Comment faire :

Pour convertir une chaîne en minuscules en utilisant Gleam, vous pouvez utiliser la fonction `String.to_lower`. Elle prend une chaîne en tant qu'argument et renvoie la même chaîne avec tous les caractères en minuscules. Voici un exemple de code :

```Gleam
let string = "HELLO WORLD"
let lower_case_string = String.to_lower(string)
```

Lorsque vous exécutez ce code, la valeur de `lower_case_string` sera "hello world".

## Plongée en profondeur :

La conversion de chaîne en minuscules n'est pas une fonction spécifique à Gleam, elle est disponible dans la plupart des langages de programmation. Cela remonte à l'ASCII, un codage de caractères utilisé pour représenter les symboles de texte. Dans l'ASCII, les codes numériques pour les lettres majuscules et minuscules ont une différence de 32. C'est pourquoi la conversion de chaîne en minuscules est également appelée "baisser la casse" ou "changer la casse".

Dans Gleam, il existe également la fonction `String.to_upper` pour convertir une chaîne en majuscules. Si vous voulez ignorer les accents lors de la conversion, vous pouvez utiliser `String.to_ascii_upper` et `String.to_ascii_lower`.

## À voir aussi :

Si vous voulez en savoir plus sur les fonctionnalités disponibles pour les chaînes de caractères en Gleam, consultez la documentation officielle sur les chaînes. Vous pouvez également apprendre différentes façons de modifier le cas des chaînes dans d'autres langages de programmation, tels que Python ou JavaScript.