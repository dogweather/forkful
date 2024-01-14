---
title:                "PHP: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsqu'on programme en PHP, il est souvent nécessaire de faire des modifications massives sur du texte. Cela peut être pour corriger des erreurs, mettre à jour du contenu ou simplement pour adapter le texte à un nouveau format. Mais comment faire ces recherches et remplacements de manière efficace ? C'est ce que nous allons découvrir dans cet article.

## Comment faire

Pour faire une recherche et un remplacement de texte en PHP, il existe plusieurs fonctions qui peuvent être utilisées en fonction de vos besoins.

La plus simple est la fonction `str_replace()` qui permet de remplacer toutes les occurrences d'un motif dans une chaîne de caractères. Voici un exemple de code :

```PHP
$fruits = "pomme, banane, poire";
$nouveaux_fruits = str_replace("pomme", "fraise", $fruits);
echo $nouveaux_fruits; //affiche "fraise, banane, poire"
```

Il est également possible d'utiliser des expressions régulières pour des recherches et remplacements plus complexes avec la fonction `preg_replace()`. Voici un exemple :

```PHP
$phrase = "J'aime les langages de programmation";
$nouvelle_phrase = preg_replace("/langages de programmation/", "fraisiers", $phrase);
echo $nouvelle_phrase; //affiche "J'aime les fraisiers"
```

Enfin, pour faire une recherche et un remplacement dans un fichier, il est conseillé d'utiliser la fonction `file_get_contents()` pour récupérer le contenu, d'effectuer les modifications avec les fonctions précédentes et de réécrire le fichier avec la fonction `file_put_contents()`.

## Plongée en profondeur

Pour une maîtrise totale de la recherche et du remplacement de texte en PHP, il est important de connaître les différentes options disponibles pour les fonctions `str_replace()` et `preg_replace()`.

Par exemple, la fonction `str_replace()` peut accepter des tableaux pour les paramètres de remplacement, permettant ainsi de remplacer plusieurs motifs différents par leur équivalent en une seule opération. La fonction `preg_replace()` offre également de nombreuses options, telles que l'utilisation de fonctions de rappel pour la transformation des motifs, ou encore la possibilité d'ignorer la casse des caractères.

Il est donc conseillé de bien se documenter sur ces fonctions pour les utiliser au mieux en fonction de vos besoins spécifiques.

## Voir aussi

- [Documentation officielle de PHP sur str_replace()](https://www.php.net/manual/fr/function.str-replace.php)
- [Documentation officielle de PHP sur preg_replace()](https://www.php.net/manual/fr/function.preg-replace.php)
- [Article sur les expressions régulières en PHP](https://www.php.net/manual/fr/book.pcre.php)