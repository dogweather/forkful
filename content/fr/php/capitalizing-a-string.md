---
title:                "PHP: Majuscule d'une chaîne de caractères"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Pourquoi: Il peut sembler banal de capitaliser une chaîne de caractères en PHP, mais il est en réalité très utile pour améliorer l'apparence de vos données et les rendre plus lisibles pour vos utilisateurs.

Comment faire: Voici un exemple simple de code pour capitaliser une chaîne de caractères en PHP :

```PHP
$maChaine = "mon texte en minuscules";
echo strtoupper($maChaine);
```

Le résultat de ce code sera :

```PHP
MON TEXTE EN MINUSCULES
```

Voici un autre exemple avec un peu plus de manipulations :

```PHP
$maChaine = "mon texte en minuscules";
$maChaine = strtolower($maChaine); // convertit en minuscules
echo ucfirst($maChaine); // met en majuscule la première lettre
```

Le résultat sera cette fois-ci :

```PHP
Mon texte en minuscules
```

Profonde plongée: Vous vous demandez peut-être comment la fonction `strtoupper()` fonctionne exactement ? Elle prend une chaîne de caractères en entrée et la transforme en majuscules selon les règles de la langue utilisée par PHP. Cela signifie que si vous utilisez des caractères spéciaux, ceux-ci seront également convertis en majuscules si vous utilisez cette fonction.

Pour convertir une chaîne de caractères en minuscules, vous pouvez utiliser la fonction `strtolower()`. Si vous voulez capitaliser seulement la première lettre d'une chaîne, vous pouvez utiliser la fonction `ucfirst()`. Il existe également d'autres méthodes pour capitaliser une chaîne telle que `ucwords()` qui mettra en majuscule la première lettre de chaque mot dans la chaîne.

Il est également important de noter que ces fonctions peuvent être utilisées avec toutes les langues supportées par PHP, ce qui en fait un outil très pratique pour un développement multilingue.

Voir aussi: Pour en savoir plus sur les différentes fonctions disponibles pour manipuler les chaînes de caractères en PHP, vous pouvez consulter la documentation officielle : 

- `strtoupper()` : http://php.net/manual/fr/function.strtoupper.php
- `strtolower()` : http://php.net/manual/fr/function.strtolower.php 
- `ucfirst()` : http://php.net/manual/fr/function.ucfirst.php 
- `ucwords()` : http://php.net/manual/fr/function.ucwords.php