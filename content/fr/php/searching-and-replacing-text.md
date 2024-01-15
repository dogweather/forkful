---
title:                "Recherche et remplacement de texte"
html_title:           "PHP: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi


Si vous êtes un développeur PHP, il est très probable que vous auriez à travailler avec du texte à un moment donné. L'une des tâches les plus courantes consiste à trouver et remplacer certaines parties de ce texte. Cela peut sembler fastidieux, mais grâce à la puissance de PHP, cela peut être fait de manière rapide et efficace.

## Comment Faire

La fonction "str_replace" de PHP est un moyen simple et efficace pour trouver et remplacer du texte dans une chaîne de caractères. Voici un exemple de code montrant comment cela fonctionne :

```PHP
$texte = "Bonjour tout le monde !";
$modification = str_replace("Bonjour", "Salut", $texte);
echo $modification;
```

Le code ci-dessus remplace "Bonjour" par "Salut" dans la chaîne de caractères "$texte" et affiche le résultat "Salut tout le monde !". Vous pouvez également utiliser la fonction "str_ireplace" pour une recherche sans casse, ce qui signifie qu'elle ne fera pas la différence entre les majuscules et les minuscules lors de la recherche.

Vous pouvez également utiliser des tableaux pour remplacer plusieurs parties du texte en une seule fois. Voici un autre exemple de code :

```PHP
$texte = "C'est un très beau jour.";
$recherche = array("très beau", "jour");
$remplacement = array("magnifique", "matin");
$modification = str_replace($recherche, $remplacement, $texte);
echo $modification;
```

Le résultat sera "C'est un magnifique matin."

## Développement Approfondi

Il y a plusieurs autres fonctions de PHP qui peuvent être utiles pour trouver et remplacer du texte. La fonction "strtr" permet de remplacer un ensemble de caractères par un autre ensemble de caractères spécifié dans un tableau. La fonction "preg_replace" utilise des expressions régulières pour trouver et remplacer des parties du texte. 

Une autre fonction utile est "substr_replace", qui permet de remplacer une partie spécifiée du texte par une autre chaîne de caractères, en utilisant des paramètres pour déterminer l'emplacement de la partie à remplacer.

Il est également important de noter que les fonctions de recherche et de remplacement de PHP sont sensibles aux encodages de caractères. Si vous travaillez avec des langues non latines, il est important de prendre en compte les encodages pour éviter des résultats inattendus.

## Voir Aussi

Si vous souhaitez en savoir plus sur les fonctions de recherche et de remplacement de PHP, vous pouvez consulter la documentation officielle sur le site de PHP.net. Vous pouvez également trouver des ressources en ligne telles que des tutoriels et des forums de développeurs où vous pouvez poser des questions et en apprendre davantage sur ces fonctions.

Liens utiles:

- Documentation sur "str_replace" : https://www.php.net/manual/fr/function.str-replace.php
- Documentation sur "str_ireplace": https://www.php.net/manual/fr/function.str-ireplace.php
- Documentation sur "strtr": https://www.php.net/manual/fr/function.strtr.php
- Documentation sur "substr_replace": https://www.php.net/manual/fr/function.substr-replace.php
- Documentation sur "preg_replace": https://www.php.net/manual/fr/function.preg-replace.php