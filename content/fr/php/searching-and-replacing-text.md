---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## C'est quoi & Pourquoi?
La recherche et le remplacement de texte sont des fonctions essentielles dans la programmation. Ces opérations sont utilisées pour trouver une chaîne spécifique dans le texte et l'échanger avec une autre, ce qui est utile dans divers scénarios tels que la correction des erreurs d'orthographe, le renommage des variables, et plus encore.

## Comment faire:
PHP fournit différentes méthodes pour chercher et remplacer du texte. Nous allons voir deux fonctions principales : `str_replace()` et `preg_replace()`.
```PHP 
<?php
// Utilisation de str_replace()
$texte = "Bonjour, je suis un développeur!";
$texteNouveau = str_replace("développeur", "programmeur", $texte);
echo $texteNouveau; 
// sorties "Bonjour, je suis un programmeur!"

// Utilisation de preg_replace()
$texte = "Le cours PHP est à 15h30.";
$texteNouveau = preg_replace("/([0-9]{2}h[0-9]{2})/", "16h30", $texte);
echo $texteNouveau; 
// sorties "Le cours PHP est à 16h30."
?>
```
## Plongée profonde
Historiquement, la recherche et le remplacement de texte étaient réalisées manuellement, ce qui était une tâche titanesque pour les gros fichiers. L'évolution de la programmation a permis l'automatisation de ce processus. En PHP, `str_replace()` et `preg_replace()` font partie des fonctions de base. `str_replace()` est direct et simple tandis que `preg_replace()` utilise des expressions régulières, offrant plus de flexibilité mais aussi plus de complexité.

Il existe des alternatives comme `str_ireplace()` pour un remplacement insensible à la casse, et `substr_replace()` pour un remplacement partiel. Quant à l'implémentation, ces fonctions parcourent le texte, trouvent la correspondance, puis font le remplacement - tout cela en quelques nano-secondes.

## Voir aussi
Pour plus d'informations, visitez la [Documentation officielle PHP](https://www.php.net/manual/en/book.strings.php) pour les fonctions de chaîne. Vous pouvez également consulter le [PCRE Pattern Syntax](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php) pour tout comprendre sur les expressions régulières.