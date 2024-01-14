---
title:    "PHP: Extraction de sous-chaînes"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une méthode utile pour manipuler des chaînes de caractères dans un programme PHP. Que vous ayez besoin de récupérer une partie spécifique d'une chaîne ou de remplacer un ensemble de caractères, cette fonctionnalité vous sera très utile.

## Comment faire

Pour extraire une sous-chaîne, utilisez la fonction `substr()` en lui passant en paramètres la chaîne originale, l'index de départ et la longueur de la sous-chaîne que vous souhaitez obtenir. Voici un exemple de code :

```PHP
<?php

$chaine = "Bonjour tout le monde";

$sous_chaine = substr($chaine, 8, 5);

echo $sous_chaine;

// Output: tout
```

Dans cet exemple, nous avons utilisé la fonction `substr()` pour extraire une sous-chaîne de la 8ème à la 12ème position (5 caractères) de la chaîne principale. Vous pouvez également utiliser des valeurs négatives pour les deux derniers paramètres afin d'extraire une sous-chaîne à partir de la fin de la chaîne.

Vous pouvez également remplacer une partie d'une chaîne en utilisant la fonction `substr_replace()`. Voici un exemple :

```PHP
<?php

$chaine = "Bonjour tout le monde";

$nouvelle_chaine = substr_replace($chaine, "soir", 8, 4);

echo $nouvelle_chaine;

// Output: Bonsoir tout le monde
```

Dans cet exemple, nous avons utilisé `substr_replace()` pour remplacer la sous-chaîne "tout" par "soir" à partir de la 8ème position de la chaîne principale.

## Plongeons plus en profondeur

L'extraction de sous-chaînes peut également être utilisée pour effectuer des opérations plus complexes, comme la recherche d'une sous-chaîne à l'aide d'une expression régulière. La fonction `preg_match()` peut être utilisée pour cela.

Voici un exemple de code qui utilise `preg_match()` pour extraire toutes les occurrences de chiffres d'une chaîne :

```PHP
<?php

$chaine = "J'ai 3 pommes et 5 bananes";

preg_match_all('!\d+!', $chaine, $matches);

print_r($matches[0]);

// Output: Array ( [0] => 3 [1] => 5 )
```

Comme vous pouvez le voir, la fonction `preg_match()` a retourné un tableau contenant toutes les occurrences de chiffres dans la chaîne.

## Voir aussi

- [Documentation officielle de la fonction substr() en français](https://www.php.net/manual/fr/function.substr.php)
- [Documentation officielle de la fonction substr_replace() en français](https://www.php.net/manual/fr/function.substr-replace.php)
- [Documentation officielle de la fonction preg_match() en français](https://www.php.net/manual/fr/function.preg-match.php)