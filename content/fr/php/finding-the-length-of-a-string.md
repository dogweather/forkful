---
title:    "PHP: Trouver la longueur d'une chaîne de caractères"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Le calcul de la longueur d'une chaîne de caractères est une tâche courante lors de la programmation en PHP. Connaître la longueur d'une chaîne peut être utile dans de nombreuses situations, notamment pour afficher une limite de caractères dans un formulaire ou pour vérifier si une chaîne dépasse une certaine taille.

## Comment Faire

Pour trouver la longueur d'une chaîne en PHP, nous pouvons utiliser la fonction `strlen( )` en passant la chaîne en paramètre. Voici un exemple de code qui illustre cela:

```PHP
$chaine = "Bonjour le monde!";
echo strlen($chaine);
```

Cela va afficher `17`, car il y a 17 caractères dans la chaîne "Bonjour le monde!". Nous pouvons également utiliser `mb_strlen( )` pour traiter correctement les caractères multibyte tels que les caractères accentués.

```PHP
$chaine = "Bonjour le monde!";
echo mb_strlen($chaine);
```

Ce code affichera également `17` car il compte correctement les caractères multibyte.

## Plongée Profonde

Il est important de noter que `strlen( )` et `mb_strlen( )` ne comptent pas seulement les caractères visibles, mais aussi les caractères de contrôle et les espaces blancs. Par exemple, si nous avons une chaîne avec un espace blanc à la fin, la longueur sera différente selon la fonction utilisée.

```PHP
$chaine = "Bonjour le monde!  ";
echo strlen($chaine); // Affiche 19
echo mb_strlen($chaine); // Affiche 20
```

Il est également intéressant de noter que les chaînes en PHP peuvent avoir une longueur maximale de 2GB en raison de la limitation des systèmes d'exploitation. Cela signifie que si vous avez une chaîne qui dépasse 2GB, la fonction `strlen( )` renverra une valeur incorrecte.

À présent, vous savez comment trouver la longueur d'une chaîne en PHP et vous êtes conscient des subtilités à prendre en compte lors de l'utilisation de cette fonction.

## Voir Aussi

- Documentation sur `strlen()` : https://www.php.net/manual/fr/function.strlen.php
- Documentation sur `mb_strlen()` : https://www.php.net/manual/fr/function.mb-strlen.php