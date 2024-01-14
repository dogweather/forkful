---
title:    "PHP: Extraction de sous-chaînes"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes, aussi connue sous le nom de "substring", est une fonctionnalité importante en programmation PHP. Elle permet de sélectionner et d'extraire une partie spécifique d'une chaîne de caractères pour un traitement ultérieur. Cela peut être utile dans de nombreux cas, notamment pour manipuler des données ou pour générer des rapports.

## Comment faire

Pour extraire une sous-chaîne dans PHP, vous pouvez utiliser la fonction ```substr()```. Voici un exemple de code :

```PHP
$string = "Bonjour World!";
$substring = substr($string, 7); // Va extraire "World!" à partir du 7ème caractère de $string
echo $substring; // Affiche "World!"
```

Vous pouvez également préciser un deuxième argument à la fonction ```substr()``` pour indiquer jusqu'où vous souhaitez extraire la sous-chaîne. Par exemple :

```PHP
$string = "Bonjour World!";
$substring = substr($string, 2, 5); // Va extraire "njour" à partir du 2ème caractère de $string, sur une longueur de 5 caractères
echo $substring; // Affiche "njour"
```

Il est également possible d'utiliser des valeurs négatives pour les arguments de la fonction ```substr()```. Dans ce cas, la sous-chaîne sera extraite à partir de la fin de la chaîne de caractères. Par exemple :

```PHP
$string = "Bonjour World!";
$substring = substr($string, -6); // Va extraire "World!" à partir du 6ème caractère en partant de la fin de $string
echo $substring; // Affiche "World!"
```

Cette fonctionnalité peut être très pratique pour manipuler des données complexes ou pour effectuer des opérations spécifiques sur une partie d'une chaîne de caractères.

## Exploration approfondie

Outre la fonction ```substr()```, PHP propose également d'autres fonctions pour extraire des sous-chaînes, telles que ```mb_substr()``` pour les chaînes multibyte ou encore ```strstr()``` pour trouver une sous-chaîne spécifique. Chacune de ces fonctions a ses propres spécificités et il peut être intéressant de prendre le temps de les explorer pour trouver celle qui correspond le mieux à vos besoins.

Il est également important de noter que l'utilisation de fonctions d'extraction de sous-chaînes peut avoir un impact sur les performances de votre code, en particulier si vous manipulez de grandes quantités de données. Dans ce cas, il peut être judicieux de rechercher des alternatives plus optimisées.

## Voir aussi

- Documentation officielle sur la fonction ```substr()```: https://www.php.net/manual/fr/function.substr.php
- Différences entre ```substr()``` et ```mb_substr()```: https://stackoverflow.com/questions/3171938/php-substr-vs-mb-substr
- Optimisation des performances en utilisant ```preg_match()```: https://medium.com/@exesse/how-to-substring-like-a-boss-in-php-9dcb4f50fddc