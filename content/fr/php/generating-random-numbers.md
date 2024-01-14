---
title:    "PHP: Génération de nombres aléatoires"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi générer des nombres aléatoires ?

Générer des nombres aléatoires est une compétence essentielle pour tout programmeur PHP. Que ce soit pour créer des jeux, des simulations ou des tests de performances, avoir la capacité de générer des nombres aléatoires est crucial pour de nombreuses applications. Dans cet article, nous allons vous montrer comment générer des nombres aléatoires en utilisant PHP.

## Comment faire

Pour générer des nombres aléatoires en utilisant PHP, nous allons utiliser la fonction `rand()`. Cette fonction prend deux paramètres, le premier étant le nombre minimum que vous souhaitez générer et le deuxième étant le nombre maximum.

```PHP
<?php
// Génère un nombre aléatoire entre 1 et 10
$num = rand(1, 10);
echo $num;
// Output: 7
```

En utilisant cette fonction, vous pouvez générer des nombres aléatoires dans différentes gammes et les utiliser dans votre code pour effectuer une action aléatoire.

## Plongeon en profondeur

En utilisant `rand()`, vous pouvez également générer des nombres aléatoires avec des décimales en utilisant la fonction `mt_rand()` en combinaison avec `mt_getrandmax()`.

```PHP
<?php
// Génère un nombre aléatoire avec 2 décimales entre 0 et 1
$num = mt_rand() / mt_getrandmax();
echo $num;
// Output: 0.5432
```

De plus, il est important de noter que la fonction `rand()` utilise un algorithme pseudo-aléatoire, ce qui signifie que les nombres générés peuvent ne pas être complètement aléatoires. Pour obtenir des nombres aléatoires plus fiables, vous pouvez utiliser la fonction `random_int()` introduite dans PHP 7.

## Voir aussi

Maintenant que vous avez compris comment générer des nombres aléatoires en utilisant PHP, vous pouvez consulter ces liens pour en savoir plus sur les différents algorithmes de génération de nombres aléatoires et leur utilisation dans des applications pratiques :

- [Génération de nombres aléatoires en PHP](https://www.php.net/manual/fr/function.random.php)
- [Pourquoi les nombres aléatoires sont importants en programmation](https://code.tutsplus.com/fr/tutorials/the-importance-of-random-numbers-in-programming--cms-28552)
- [Sécurité des nombres aléatoires en PHP](https://medium.com/@ezraroi/how-secure-is-the-random-number-generator-of-php-2016-edition-a0b398a50539)