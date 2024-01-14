---
title:    "PHP: Majuscule d'une chaîne de caractères"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

L'une des choses les plus courantes que les programmeurs font est de manipuler des chaînes de caractères, et l'une de ces manipulations est la mise en majuscule d'une chaîne. Mais pourquoi devrions-nous le faire? Eh bien, il peut y avoir plusieurs raisons pour lesquelles nous pourrions vouloir capitaliser une chaîne. Peut-être que nous voulons que les noms soient affichés en majuscule pour plus de lisibilité, ou peut-être que nous voulons formatter des informations pour les rendre plus cohérentes. Quelle que soit la raison, la capitalisation des chaînes est une compétence essentielle à avoir en tant que programmeur.

## Comment faire

Heureusement, en PHP, il existe une fonction intégrée qui rend la capitalisation d'une chaîne de caractères très simple. La fonction "ucfirst" prend une chaîne en entrée et renvoie cette même chaîne avec la première lettre en majuscule. Examinons un exemple de code pour mieux comprendre:

```PHP
$nom = "emilie";
echo ucfirst($nom);
```

La sortie de ce code serait "Emilie". Nous pouvons également utiliser la fonction "ucwords" pour capitaliser chaque mot d'une chaîne. Regardons un autre exemple:

```PHP
$phrase = "la belle journée";
echo ucwords($phrase);
```

La sortie de ce code serait "La Belle Journée". Comme vous pouvez le voir, ces fonctions sont très utiles pour la mise en majuscule de chaînes de caractères.

## Deep Dive

Maintenant, intéressons-nous un peu plus à la fonction "ucfirst". Cette fonction a en fait une variante appelée "lcfirst" qui fait exactement le contraire. Elle prend une chaîne en entrée et renvoie cette même chaîne avec la première lettre en minuscule. Cette fonction peut être utile si vous avez besoin de modifier une chaîne contenant un prénom ou un nom de famille avec une majuscule en première lettre. Vous pouvez également utiliser la fonction "strtolower" pour mettre une chaîne entière en minuscule.

Une autre chose importante à noter est que ces fonctions fonctionnent différemment selon les langages et les alphabets. En PHP par exemple, la fonction "ucfirst" ne prend pas en compte les accents, tandis que la fonction "ucwords" les prend en compte. Il est donc important de vérifier comment ces fonctions se comportent avec les caractères spéciaux dans votre environnement de programmation.

## Voir aussi

Si vous souhaitez en savoir plus sur la manipulation des chaînes de caractères en PHP, voici quelques liens utiles:

- [Documentation officielle de la fonction "ucfirst"](https://www.php.net/manual/fr/function.ucfirst.php)
- [Documentation officielle de la fonction "ucwords"](https://www.php.net/manual/fr/function.ucwords.php)
- [Tutoriel sur la manipulation des chaînes de caractères en PHP](https://www.pierre-giraud.com/php-mysql-apprendre-coder-cours/fonction-manipulation-chaine-caracteres-php/)

Maintenant que vous avez appris à capitaliser des chaînes en PHP, vous pouvez l'appliquer dans vos projets et rendre vos données plus cohérentes et plus lisibles. N'hésitez pas à explorer d'autres fonctions de manipulation de chaînes pour perfectionner vos compétences en programmation. Bonne chance!