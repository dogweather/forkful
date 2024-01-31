---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
simple_title:         "Utilisation des expressions régulières"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Les expressions régulières (regex) filtrent et manipulent du texte. Les dev utilisent des regex pour chercher, valider, et modifier des chaînes rapidement.

## Comment faire :
```php
<?php
$texte = "01-01-2023";
$pattern = "/^(\d{2})-(\d{2})-(\d{4})$/";

// Recherche de la date
if (preg_match($pattern, $texte, $matches)) {
    echo "Jour: " . $matches[1] . ", Mois: " . $matches[2] . ", Année: " . $matches[3];
} else {
    echo "Format invalide.";
}
// Sortie: Jour: 01, Mois: 01, Année: 2023

// Remplacement de texte
$texteModifie = preg_replace("/\bweb\b/", "Web", "Le développement web est cool.");
echo $texteModifie;
// Sortie: Le développement Web est cool.
?>
```

## Exploration en profondeur
Historique : Les regex existent depuis les années 1950, formalisées par Stephen Kleene. En PHP, `preg_*` est basé sur la bibliothèque PCRE (Perl Compatible Regular Expressions).

Alternatives : Sans regex, on a `strpos`, `str_replace`, mais ils sont limités. Pour des opérations complexes, regex est incontournable.

Implémentation : PHP utilise un backtracking algorithm pour les regex, efficace mais gare aux expressions mal conçues—elles peuvent ralentir le script.

## Voir aussi
- [PHP Manual on PCRE](https://www.php.net/manual/en/book.pcre.php)
- [Regex101](https://regex101.com/) pour tester les regex en ligne
- [O'Reilly's "Mastering Regular Expressions"](http://shop.oreilly.com/product/9780596528126.do) pour maîtriser le sujet
