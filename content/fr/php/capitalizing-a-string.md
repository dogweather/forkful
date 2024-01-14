---
title:    "PHP: Capitaliser une chaîne de caractères"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données textuelles en PHP, il est probable que vous ayez besoin de capitaliser une chaîne de caractères à un moment donné. Cela signifie simplement mettre la première lettre en majuscule et le reste en minuscules. Mais pourquoi est-ce important et comment le faire efficacement ? Dans cet article, nous allons examiner pourquoi et comment capitaliser une chaîne en PHP.

## Comment le faire

```PHP
$string = "bonjour tout le monde";
echo ucwords($string);

// Output: Bonjour Tout Le Monde
```

Comme vous pouvez le voir dans l'exemple ci-dessus, la fonction intégrée ```ucwords()``` prend une chaîne de caractères en argument et la transforme en la capitalisant. Vous pouvez également utiliser la fonction ```ucfirst()``` pour capitaliser seulement la première lettre de la chaîne.

```PHP
$string = "bonjour tout le monde";
echo ucfirst($string);

// Output: Bonjour tout le monde
```

N'oubliez pas que ces fonctions ne modifient pas la chaîne d'origine, elles renvoient plutôt une nouvelle chaîne avec les modifications appliquées. Vous pouvez donc les utiliser directement dans un ```echo``` ou les stocker dans une variable pour une utilisation ultérieure.

## Deep Dive

Mais comment ces fonctions fonctionnent-elles réellement ? En PHP, les chaînes de caractères sont traitées comme des tableaux de caractères individuels. La fonction ```ucwords()``` parcourt chaque mot dans la chaîne et applique la fonction ```ucfirst()``` à chaque mot, tandis que la fonction ```ucfirst()``` applique simplement la fonction ```strtoupper()``` sur le premier caractère.

Il est également important de noter que ces fonctions ne prennent pas en compte les accents et les caractères spéciaux. Si vous avez besoin de capitaliser correctement des caractères non-ASCII, vous devrez utiliser une autre méthode.

## Voir aussi

- [La documentation officielle de ucwords() en français](https://www.php.net/manual/fr/function.ucwords.php)
- [Différences entre ucfirst() et ucwords()](https://www.thoughtco.com/ucfirst-versus-ucwords-in-php-2693583)
- [Une façon alternative de capitaliser des chaînes en utilisant mb_convert_case()](https://www.php.net/manual/fr/function.mb-convert-case.php)

Cela conclut notre article sur la capitalisation des chaînes en PHP. J'espère que vous avez trouvé cet article utile. N'hésitez pas à expérimenter avec ces fonctions et à consulter la documentation officielle pour en savoir plus. À bientôt pour plus d'astuces de programmation en PHP !