---
title:                "PHP: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi générer des nombres aléatoires?

La génération de nombres aléatoires est une fonctionnalité couramment utilisée en programmation pour effectuer des tâches telles que la sélection aléatoire d'éléments dans une liste ou la création de données de test aléatoires. Elle peut également être utilisée pour ajouter une touche de hasard et de dynamisme à une application.

## Comment faire

La génération de nombres aléatoires en PHP est très simple grâce à la fonction `rand()`. Elle prend deux paramètres optionnels, le premier étant la valeur minimale et le second la valeur maximale. Voici un exemple de code en PHP pour générer 5 nombres aléatoires entre 1 et 10 :

```PHP
<?php
// Génère 5 nombres aléatoires entre 1 et 10
for ($i = 0; $i < 5; $i++) {
  $rand_num = rand(1, 10);
  echo "Nombre aléatoire #" . ($i + 1) . ": $rand_num\n";
}
```

**Sortie:**

```
Nombre aléatoire #1: 7
Nombre aléatoire #2: 4
Nombre aléatoire #3: 10
Nombre aléatoire #4: 2
Nombre aléatoire #5: 9
```

## Plongée en profondeur

Bien qu'elle soit simple à utiliser, la fonction `rand()` peut produire des résultats prévisibles si elle est utilisée de manière inappropriée. Cela peut poser des problèmes en matière de sécurité si les nombres aléatoires sont utilisés pour générer des mots de passe ou des jetons d'authentification. Pour éviter cela, il est recommandé d'utiliser la fonction `random_int()` qui utilise un générateur cryptographiquement sécurisé pour produire des nombres aléatoires.

En outre, il est important de noter que la fonction `rand()` utilise un algorithme de génération de nombres pseudo-aléatoires, ce qui signifie qu'elle produit des nombres qui peuvent sembler aléatoires, mais en réalité ils sont déterminés par une formule mathématique. Si vous avez besoin de nombres vraiment aléatoires, il est conseillé d'utiliser un générateur externe de nombres aléatoires, tel que le module OpenSSL ou un service de génération de nombres aléatoires en ligne.

## Voir aussi

- [Documentation PHP sur la fonction `rand()`](https://www.php.net/manual/en/function.rand.php)
- [Documentation PHP sur la fonction `random_int()`](https://www.php.net/manual/en/function.random-int.php)
- [Random.org - Service de génération de nombres aléatoires en ligne](https://www.random.org/)