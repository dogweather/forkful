---
title:                "PHP: Recherche et remplacement de texte"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en PHP, vous savez probablement qu'une tâche courante est de modifier du texte dans une chaîne de caractères. Que vous cherchiez à remplacer un mot spécifique ou à effectuer des modifications plus complexes, il est important de connaître les différentes options disponibles pour effectuer cette tâche efficacement. Dans cet article, nous allons vous montrer comment rechercher et remplacer du texte en utilisant PHP.

## Comment Faire

Pour rechercher et remplacer du texte dans une chaîne de caractères, vous pouvez utiliser la fonction `str_replace()` en PHP. Elle prend trois paramètres : la chaîne à rechercher, la chaîne de remplacement et la chaîne dans laquelle effectuer la recherche et le remplacement. Voici un exemple de code :

```PHP
$originalString = "Bonjour à tous!";
$newString = str_replace("Bonjour", "Hello", $originalString);

echo $newString; // affiche : Hello à tous!
```

Comme vous pouvez le voir, la fonction `str_replace()` remplace la chaîne "Bonjour" par "Hello". Vous pouvez également utiliser des tableaux pour remplacer plusieurs occurrences d'une seule fois :

```PHP
$originalString = "Le chat et le chien sont amis.";
$oldWords = array("chat", "chien");
$newWords = array("chaton", "chiot");

$newString = str_replace($oldWords, $newWords, $originalString);

echo $newString; // affiche : Le chaton et le chiot sont amis.
```

Vous pouvez également utiliser la fonction `preg_replace()` pour effectuer des modifications plus complexes en utilisant des expressions régulières. Voici un exemple :

```PHP
$originalString = "Les jeunes aiment le football.";
$newString = preg_replace("/football/", "basketball", $originalString);

echo $newString; // affiche : Les jeunes aiment le basketball.
```

## Exploration en Profondeur

La fonction `str_replace()` est très utile et efficace pour la plupart des scénarios de recherche et de remplacement de texte. Cependant, il est important de comprendre comment elle fonctionne en interne. En effet, cette fonction n'est pas sensible à la casse, c'est-à-dire qu'elle ne prend pas en compte les majuscules et les minuscules lors de la recherche. De plus, elle remplace toutes les occurrences de la chaîne recherchée, même si elles se trouvent à l'intérieur d'une autre chaîne.

Si vous avez besoin de plus de contrôle sur la sensibilité à la casse ou sur le nombre d'occurrences remplacées, vous pouvez utiliser la fonction `str_ireplace()` qui est sensible à la casse et la fonction `substr_replace()` pour ne remplacer qu'une seule occurrence.

## Voir Aussi

Pour en savoir plus sur la manipulation de chaînes de caractères en PHP, vous pouvez consulter les ressources suivantes :

- [Documentation PHP sur la fonction str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [Documentation PHP sur la fonction preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Documentation PHP sur la fonction str_ireplace()](https://www.php.net/manual/en/function.str-ireplace.php)
- [Documentation PHP sur la fonction substr_replace()](https://www.php.net/manual/en/function.substr-replace.php)