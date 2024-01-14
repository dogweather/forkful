---
title:                "PHP: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut être utile dans différents scénarios de programmation, tels que le nettoyage de données ou la manipulation de chaînes de caractères. Cela peut également être utile pour optimiser le traitement de données.

## Comment faire

Pour supprimer des caractères correspondant à un motif en utilisant PHP, vous pouvez utiliser la fonction `preg_replace()`. Cette fonction prend trois arguments : le motif à rechercher, le texte dans lequel le motif sera recherché et le texte de remplacement par lequel le motif sera remplacé. Voici un exemple de code qui utilise `preg_replace()` pour supprimer des caractères correspondant à un motif dans une chaîne de caractères :

```PHP
<?php
// Le texte original
$texte = "Cette chaîne contient des voyelles.";

// Supprimer les voyelles (a, e, i, o, u)
$texte_modifie = preg_replace("/[aeiou]/i", "", $texte);

// Afficher le résultat
echo $texte_modifie;
```

Résultat :

```
Ct chn cntnt ds vylls.
```

## Plongée en profondeur

La fonction `preg_replace()` utilise des expressions régulières pour rechercher et remplacer des motifs dans une chaîne de caractères. Les expressions régulières sont des modèles de texte qui permettent de définir des motifs à rechercher dans une chaîne. Vous pouvez utiliser différents symboles et opérateurs dans une expression régulière pour créer des motifs plus complexes.

Par exemple, pour supprimer tous les nombres dans une chaîne de caractères, vous pouvez utiliser le motif `[0-9]` qui correspond à tous les chiffres de 0 à 9. Vous pouvez également utiliser la fonction `preg_replace()` pour remplacer des caractères par d'autres caractères spécifiques ou même par une chaîne de caractères entière.

La suppression de caractères correspondant à des motifs est une technique utile à connaître lorsque vous travaillez avec des données et des chaînes de caractères en PHP. N'hésitez pas à explorer davantage les expressions régulières pour découvrir toutes les possibilités qu'elles offrent.

## Voir aussi

- [Documentation officielle PHP : preg_replace()](https://www.php.net/manual/fr/function.preg-replace.php)
- [Tutoriel sur les expressions régulières en PHP](https://www.w3schools.com/php/php_regex.asp)
- [Cheat sheet des expressions régulières en PHP](https://www.rexegg.com/regex-php.html)