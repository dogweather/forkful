---
title:                "PHP: Utiliser des expressions régulières"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont des outils utiles pour trouver des motifs dans une chaîne de caractères. Elles peuvent être utilisées pour valider des entrées utilisateur, filtrer des données ou encore pour extraire des informations spécifiques d'un texte. En utilisant des expressions régulières, les programmeurs peuvent économiser beaucoup de temps lors de la manipulation de données.

## Comment faire

Pour utiliser des expressions régulières en PHP, vous pouvez utiliser la fonction `preg_match()`. Par exemple, si vous souhaitez trouver tous les numéros de téléphone dans une chaîne de caractères, vous pouvez utiliser l'expression régulière suivante :

```PHP
preg_match("/[0-9]{3}-[0-9]{3}-[0-9]{4}/", $texte, $resultats);
```

Cela va rechercher tous les numéros de téléphone sous le format `xxx-xxx-xxxx` dans la variable `$texte` et stocker les résultats dans le tableau `$resultats`. Vous pouvez ensuite utiliser ces résultats dans votre code pour effectuer différentes actions.

## Plongée en profondeur

Les expressions régulières peuvent sembler complexes au premier abord, mais elles peuvent être très puissantes une fois que vous avez maîtrisé leur utilisation. En plus des caractères basiques pour matcher des chiffres ou des lettres, des symboles spéciaux comme `?`, `+` ou `*` peuvent être utilisés pour identifier des motifs plus complexes. De plus, les expressions régulières peuvent utiliser des groupes de capture qui vous permettent de récupérer des sous-parties spécifiques d'un texte.

Il est important de noter que les performances peuvent être un problème lors de l'utilisation d'expressions régulières, en particulier si vous travaillez avec de gros volumes de données. Il est donc important de bien optimiser vos expressions régulières pour éviter tout ralentissement de votre programme.

## Voir aussi

- [Documentation officielle de la fonction preg_match en PHP](https://www.php.net/manual/fr/function.preg-match.php)
- [Tutoriel sur les expressions régulières en PHP](https://www.tutorialspoint.com/php/php_regular_expression.htm)
- [Expressions régulières : un outil puissant en programmation](https://www.poeme-oslo.com/ressources/guides/are/expressions-regulieres/)