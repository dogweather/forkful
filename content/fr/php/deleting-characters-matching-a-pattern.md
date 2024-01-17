---
title:                "Suppression de caractères correspondant à un modèle"
html_title:           "PHP: Suppression de caractères correspondant à un modèle"
simple_title:         "Suppression de caractères correspondant à un modèle"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font-ils?
Supprimer des caractères correspondant à un motif est une technique couramment utilisée par les programmeurs en PHP pour traiter et manipuler des chaînes de caractères. Cela permet de supprimer facilement certaines parties d'une chaîne sans avoir à écrire de longs scripts.

## Comment faire:
Voici deux exemples de code en PHP pour supprimer des caractères correspondant à un motif:

```
// Exemple 1: Suppression des espaces
$string = "Ceci est une chaîne avec des espaces.";
$pattern = '/\s/';
$new_string = preg_replace($pattern, "", $string);
echo $new_string;
// Output: "Ceciestunechaîneavecdesespaces."

// Exemple 2: Suppression des caractères spéciaux
$string2 = "Ceci est une chaîne avec des caractères spéciaux !@#$%";
$pattern2 = '/[^\w\s]/';
$new_string2 = preg_replace($pattern2, "", $string2);
echo $new_string2;
// Output: "Ceci est une chaîne avec des caractères spéciaux"
```

## Zoom en profondeur:
Cette technique de suppression de caractères a été introduite dans PHP version 4.0 et est basée sur les expressions régulières PCRE (Perl Compatible Regular Expressions). Les alternatives à cette méthode sont l'utilisation de boucles et de fonctions de manipulation de chaînes de caractères telles que str_replace() ou trim(). L'implémentation de la suppression de caractères correspondant à un motif utilise la fonction preg_replace() qui accepte trois paramètres: le motif, la chaîne de remplacement et la chaîne d'entrée.

## À voir aussi:
Pour plus d'informations sur la suppression de caractères correspondant à un motif en PHP, vous pouvez consulter la documentation officielle de PHP sur les expressions régulières ainsi que la liste complète des caractères spéciaux et des opérateurs utilisés dans les motifs PCRE.