---
title:                "PHP: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de commencer à coder en PHP, une question que l'on pourrait se poser est : pourquoi devrais-je utiliser la fonction de mise en majuscule d'une chaîne de caractères ? En réalité, il peut y avoir plusieurs raisons pour lesquelles vous pourriez avoir besoin de capitaliser une chaîne. Peut-être devez-vous formater correctement un titre ou un nom, ou peut-être voulez-vous simplement rendre le texte plus lisible.

## Comment faire

La fonction utilisée pour capitaliser une chaîne en PHP est `ucfirst()`. Elle va transformer la première lettre en majuscule et laisser les autres caractères intacts. Voyons un exemple concret :

```PHP
$nom = "martin";
echo ucfirst($nom);
```

Cet exemple va afficher "Martin" car la première lettre du nom "martin" a été mise en majuscule. Vous pouvez également utiliser la même fonction pour capitaliser la première lettre de chaque mot d'une phrase :

```PHP
$phrase = "bonjour à tous";
echo ucwords($phrase);
```

Cela va afficher "Bonjour À Tous".

## Plongée en profondeur

Outre la fonction `ucfirst()`, il existe d'autres fonctions qui peuvent vous aider à capitaliser des chaînes en PHP. La fonction `ucwords()` que nous avons utilisée dans l'exemple précédent va transformer la première lettre de chaque mot en majuscule, mais elle va également convertir les autres lettres en minuscule si nécessaire. Cela peut être utile si vous souhaitez uniformiser le format de vos chaînes de caractères.

Il existe également la fonction `strtoupper()` qui va transformer l'ensemble de la chaîne en majuscules, et la fonction `strtolower()` qui quant à elle, va mettre l'ensemble de la chaîne en minuscules. Gardez en tête que ces fonctions peuvent ne pas fonctionner comme vous le souhaitez si votre chaîne contient des caractères spéciaux ou des accents.

## Voir aussi

- [Documentation officielle de la fonction ucfirst() en PHP](https://www.php.net/manual/fr/function.ucfirst.php)
- [Liste des fonctions de manipulation de chaînes en PHP](https://www.php.net/manual/fr/ref.strings.php)