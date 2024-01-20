---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

L'extraction de sous-chaînes (substring) désigne la procédure qui permet de récupérer une portion spécifique d'une chaîne de caractères. Les programmeurs l'utilisent couramment pour manipuler et analyser des données de texte.

## Comment faire:

En PHP, nous avons deux fonctions clés pour extraire des sous-chaînes: `substr()` et `substr_compare()`. Voici comment les utiliser:

```PHP
<?php
$chaine = "Bonjour tout le monde";
$sous_chaine1 = substr($chaine, 8); // échantillon à partir de la 8ème position
echo $sous_chaine1; // affiche 'tout le monde'
$sous_chaine2 = substr($chaine, 8, 4); // échantillon 4 caractères à partir de la 8ème position
echo $sous_chaine2; // affiche 'tout'
?>
```

## Plongeons plus profondément:

L'extraction de sous-chaînes a été autour depuis les tout premiers jours de la programmation, étant un élément fondamental dans le traitement du texte. En PHP, `substr()` et `substr_compare()` sont les principales fonctions utilisées, mais il existe également `mb_substr()` pour le traitement des chaînes multibytes, utile pour les caractères non-latins.

S'il est nécessaire d'extraire des sous-chaînes basées sur des patterns, `preg_match()` et `preg_match_all()` sont des alternatives robustes qui utilisent des expressions régulières. Pour que 'substr' fonctionne correctement, il faut s'assurer que l'encodage des chaînes est cohérent, sinon, les résultats peuvent varier.

## Pour en savoir plus: 

1. Documentation PHP pour `substr()`: [https://www.php.net/manual/fr/function.substr.php](https://www.php.net/manual/fr/function.substr.php)
2. Documentation PHP pour `substr_compare()`: [https://www.php.net/manual/fr/function.substr-compare.php](https://www.php.net/manual/fr/function.substr-compare.php)
3. Tutoriel sur l'utilisation des expressions régulières en PHP: [https://www.php.net/manual/fr/book.pcre.php](https://www.php.net/manual/fr/book.pcre.php)