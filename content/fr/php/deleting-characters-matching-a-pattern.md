---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Supprimer des caractères correspondant à un motif est une opération qui consiste à éliminer de manière sélective des caractères à l'intérieur d'une chaîne de caractères. Les programmeurs la pratiquent pour manipuler les données, surtout quand ils veulent nettoyer ou formater des chaînes de caractères.

## Comment faire:

Pour supprimer des caractères correspondant à un motif en PHP, on utilise généralement la fonction `preg_replace()`. Voici comment cela fonctionne :

```PHP
<?php
    $string = "Bonjour Paris!";
    $pattern = "/[a-z]/i";
    $string = preg_replace($pattern, "", $string);
    echo $string;
?>
```

Cette partie du code affichera ' !' parce que tous les caractères alphabétiques ont été supprimés de la chaîne.

## Plongeon profond:

Historiquement, la fonction `preg_replace()` est un ajout relativement récent dans le langage PHP. Elle utilise le moteur de regex PCRE (Perl Compatible Regular Expressions) et fournit une puissante fonctionnalité de regex.

Parmi les multiples alternatives, il y a l'utilisation de `str_replace()`, qui est plus simple mais moins flexible. `str_replace()` ne permet pas l'usage des expressions régulières, mais pour les besoins de remplacement simple, elle sera plus rapide.

Côté détails d'implémentation, notez que:
- `preg_replace()` renvoie NULL si une erreur se produit.
- Il est possible d'utiliser les modificateurs de regex après le délimiteur final, comme 'i' dans notre exemple pour ignorer la casse.

## Voir aussi:

- Documentation PHP pour `preg_replace()`: [https://www.php.net/manual/fr/function.preg-replace.php](https://www.php.net/manual/fr/function.preg-replace.php)
- Documentation PHP pour `str_replace()`: [https://www.php.net/manual/fr/function.str-replace.php](https://www.php.net/manual/fr/function.str-replace.php)
- Tutoriel sur les expressions régulières en PHP: [https://www.php.net/manual/fr/book.pcre.php](https://www.php.net/manual/fr/book.pcre.php)