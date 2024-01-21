---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:40.393233-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Générer des nombres aléatoires en PHP, c'est comme lancer des dés virtuels—on obtient des valeurs imprévisibles. Les devs utilisent cela pour tout, des jeux aux simulations, en passant par la sécurité des mots de passe.

## How to: (Comment faire :)
PHP offre plusieurs fonctions pour l'aléatoire. Voici des exemples avec `rand` et `random_int`.

```PHP
<?php
// Simple numéro aléatoire entre 1 et 10
echo rand(1, 10);

// Un peu plus sûr pour la cryptographie
echo random_int(1, 10);
?>
```
Sortie possible:
```
4
7
```

## Deep Dive (Plongée en profondeur)
Historiquement, `rand()` était tout ce qu'on avait, mais il n'était pas assez sûr pour la cryptographie. Avec PHP 7, `random_int()` débarque, blindé contre les prédictions. Alternatives ? `mt_rand()` est plus rapide que `rand()` grâce à un générateur de nombres pseudo-aléatoires de Mersenne Twister. Détail intéressant, `random_bytes()` est là si tu veux de la donnée binaire aléatoire.

## See Also (Voir également)
- [PHP Manual on Random Integers](https://www.php.net/manual/en/function.random-int.php)
- [Mersenne Twister Info](https://www.php.net/manual/en/function.mt-rand.php)
- [OpenSSL for Crypto-Safe Random Bytes](https://www.php.net/manual/en/function.openssl-random-pseudo-bytes.php)