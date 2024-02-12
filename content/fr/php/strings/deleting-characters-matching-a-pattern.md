---
title:                "Suppression de caractères correspondant à un motif"
aliases:
- /fr/php/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:42:42.274560-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suppression de caractères correspondant à un motif"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)

Supprimer des caractères selon un motif, c'est retirer des parties spécifiques d'une chaîne en se basant sur un pattern (motif). Les développeurs font ça pour nettoyer les données, valider des entrées ou formater des textes.

## How to (Comment faire) :

En PHP, on utilise souvent la fonction `preg_replace` pour supprimer des caractères qui correspondent à un motif défini par une expression régulière. Voici comment ça fonctionne :

```php
<?php
$texte = "Bienvenue en 2023! PHP c'est génial.";
$motif = '/[0-9]+/';

// Supprimer les chiffres du texte
$texteModifie = preg_replace($motif, '', $texte);

echo $texteModifie; // Affiche: "Bienvenue en ! PHP c'est génial."
?>
```

On a un motif qui cherche des chiffres (`/[0-9]+/`) et on les enlève du texte.

## Deep Dive (Plongée en profondeur) :

Historiquement, PHP a toujours proposé des moyens de manipuler des chaînes de caractères, et avec l’ajout des expressions régulières (regex), il est devenu super flexible. 'preg_replace' fait partie de la suite de fonctions PCRE (Perl Compatible Regular Expressions) introduite en PHP 4.

Une alternative à `preg_replace` c'est `str_replace`, mais attention, elle ne gère pas les motifs, juste des chaînes exactes. Pour la performance, si votre motif est simple, 'str_replace' ou 'strtr' pourrait être plus rapide.

Concernant l'implémentation, `preg_replace` peut être gourmand en ressources sur des chaînes très longues ou des motifs très complexes. Utiliser un motif bien conçu est crucial pour la performance.

## See Also (Voir aussi) :

- Documentation PHP sur `preg_replace` : [php.net/manual/fr/function.preg-replace.php](https://www.php.net/manual/fr/function.preg-replace.php)
- Introduction aux expressions régulières : [regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html)
- PHP `str_replace` documentation : [php.net/manual/fr/function.str-replace.php](https://www.php.net/manual/fr/function.str-replace.php)
