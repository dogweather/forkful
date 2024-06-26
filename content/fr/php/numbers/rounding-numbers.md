---
date: 2024-01-26 03:46:00.669325-07:00
description: "Comment faire : PHP propose quelques m\xE9thodes pour arrondir les nombres\
  \ : `round()`, `ceil()` et `floor()`. Voici comment elles fonctionnent ."
lastmod: '2024-04-05T22:38:58.413644-06:00'
model: gpt-4-0125-preview
summary: "PHP propose quelques m\xE9thodes pour arrondir les nombres ."
title: Arrondir les nombres
weight: 13
---

## Comment faire :
PHP propose quelques méthodes pour arrondir les nombres : `round()`, `ceil()` et `floor()`. Voici comment elles fonctionnent :

```php
echo round(3.14159);   // Retourne 3
echo round(3.14159, 2); // Retourne 3.14

echo ceil(3.14159);    // Retourne 4, arrondit toujours à l'entier supérieur

echo floor(3.14159);   // Retourne 3, arrondit toujours à l'entier inférieur
```

## Approfondissement
L'arrondissement des nombres est essentiel en mathématiques et en calcul depuis l'Antiquité pour gérer les décimales infinies impraticables. Dans PHP, `round()` peut prendre un paramètre de précision et un mode, affectant son comportement - `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN`, etc., définissent comment il se comportera lorsqu'il rencontre un scénario ".5". La précision est clé dans les applications financières où l'arrondissement pourrait être légalement régulé, affectant la manière dont `round()` est implémenté dans le code.

Les alternatives aux fonctions intégrées incluent des méthodes d'arrondissement personnalisées ou les fonctions de BC Math pour l'arithmétique de précision arbitraire, qui sont utiles pour les scénarios nécessitant plus de contrôle ou traitant de très grands nombres où la précision native pourrait faillir.

## Voir aussi
Explorez plus dans le manuel PHP :
- [Fonction `round` de PHP](https://php.net/manual/fr/function.round.php)
- [Fonction `ceil` de PHP](https://php.net/manual/fr/function.ceil.php)
- [Fonction `floor` de PHP](https://php.net/manual/fr/function.floor.php)
- [BC Math pour l'arithmétique de précision arbitraire](https://php.net/manual/fr/book.bc.php)
