---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
(Majuscule : Quoi et Pourquoi ?)

Capitaliser une chaîne de caractères, c'est transformer la première lettre de chaque mot en majuscule. Les programmeurs l'utilisent souvent pour normaliser des titres ou des noms propres afin de préserver une constance esthétique et une lisibilité dans les interfaces utilisateurs.

## How to:
(Comment faire : )

PHP facilite la capitalisation des chaînes avec quelques fonctions prêtes à l'emploi. Voici comment utiliser `ucwords()` et `mb_convert_case()` :

```php
$texte = "bonjour le monde!";
// Capitalise chaque mot.
$texteCap = ucwords($texte);
echo $texteCap; // Affiche : Bonjour Le Monde!

// Pour gérer les caractères multioctets (comme les lettres accentuées) :
$texteUtf8 = "salut à tous!";
$texteCapUtf8 = mb_convert_case($texteUtf8, MB_CASE_TITLE, "UTF-8");
echo $texteCapUtf8; // Affiche : Salut À Tous!
```

## Deep Dive
(Exploration approfondie)

Historiquement, PHP a ajouté `ucwords()` dans les premières versions pour répondre aux besoins de mise en forme des chaines de caractères. Cependant, avec l'internationalisation, `mb_convert_case()` est devenu essentiel pour gérer correctement les caractères hors de la table ASCII standard, comme les accents.

Alternativement, certains préfèrent travailler au niveau des tableaux pour plus de contrôle :

```php
$texteArray = explode(' ', $texteUtf8);
$texteArray = array_map(function($mot){
    return mb_convert_case($mot, MB_CASE_TITLE, "UTF-8");
}, $texteArray);
echo implode(' ', $texteArray); // Salut À Tous!
```

Cette technique décompose et reconstruit les chaînes, offrant une flexibilité pour appliquer des règles de capitalisation plus spécifiques.

## See Also
(Voir aussi)

- La [documentation officielle de PHP](https://www.php.net/manual/fr/function.ucwords.php) sur `ucwords()`.
- La [documentation officielle de PHP](https://www.php.net/manual/fr/function.mb-convert-case.php) sur `mb_convert_case()`.
- Un [guide sur l'utilisation des expressions régulières en PHP](https://www.php.net/manual/fr/reference.pcre.pattern.syntax.php) si vous voulez plus de contrôle dans la manipulation des chaînes.
