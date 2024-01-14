---
title:                "PHP: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Extraction de sous-chaînes en PHP peut sembler être une tâche simple, mais il peut être très utile dans de nombreuses situations. Par exemple, vous pouvez extraire une partie spécifique d'une chaîne de caractères pour l'utiliser comme identifiant unique ou pour effectuer des manipulations spécifiques sur ces données.

## Comment faire

L'extraction de sous-chaînes en PHP peut être réalisée avec la fonction `substr ()` qui prend trois arguments : la chaîne d'origine, l'index de début et la longueur de la sous-chaîne souhaitée. Voici un exemple de code pour extraire les trois premiers caractères d'une chaîne :

```PHP
<?php
$chaine = "Bonjour tout le monde";
$sous_chaine = substr($chaine, 0, 3);
echo $sous_chaine; // Résultat : "Bon"
?>
```

Vous pouvez également utiliser des nombres négatifs pour extraire une partie de la fin de la chaîne. Par exemple, pour extraire les trois derniers caractères :

```PHP
$sous_chaine = substr($chaine, -3);
echo $sous_chaine; // Résultat : "nde"
```

Il est également possible d'utiliser la fonction `mb_substr ()` pour extraire des sous-chaînes contenant des caractères multibytes, comme les caractères accentués en français. Il suffit d'ajouter l'argument facultatif pour spécifier l'encodage. Par exemple :

```PHP
$sous_chaine = mb_substr($chaine, 0, 3, 'UTF-8');
echo $sous_chaine; // Résultat : "Bon"
```

## Plongée en profondeur

La fonction `substr ()` peut également être utilisée pour extraire une partie d'une chaîne en fonction d'un motif spécifique. Par exemple, si vous souhaitez extraire le nom d'une personne à partir d'une adresse e-mail, vous pouvez utiliser la fonction `strpos ()` pour trouver la position de l'arobase (@) et récupérer tous les caractères avant cette position.

De plus, vous pouvez utiliser d'autres fonctions de traitement de chaîne telles que `explode ()` ou `preg_match ()` pour extraire des sous-chaînes plus complexes en fonction de critères spécifiques.

## Voir aussi

- [Documentation officielle de substr ()](https://www.php.net/manual/fr/function.substr.php)
- [Exemples d'utilisation de la fonction substr ()](https://www.w3schools.com/php/func_string_substr.asp)
- [Autres fonctions de traitement de chaîne en PHP](https://www.php.net/manual/fr/ref.strings.php)