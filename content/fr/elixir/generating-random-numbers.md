---
title:                "Génération de nombres aléatoires"
html_title:           "Elixir: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Générer des nombres aléatoires est une fonctionnalité importante en programmation pour créer des jeux, des simulations, ou tout autre cas où des nombres aléatoires sont nécessaires. Les développeurs peuvent utiliser cette fonctionnalité pour créer des situations imprévisibles ou pour tester le comportement de leur code.

## Comment:
Voici un exemple de code en ```Elixir``` pour générer un nombre aléatoire entre 1 et 10:

```
rand(1..10)
```

Le résultat pourrait être, par exemple, ```7```.
Il est également possible de générer un nombre aléatoire d'un certain type de données, par exemple un booléen:
```
rand(true..false)
```
Le résultat pourrait être, par exemple, ```true``` ou ```false```.

## Plongée en Profondeur:
L'utilisation de nombres aléatoires en informatique remonte à la Seconde Guerre mondiale, lorsque les ordinateurs étaient utilisés pour générer des codes cryptographiques. De nos jours, il y a de nombreuses bibliothèques de génération de nombres aléatoires disponibles pour de nombreux langages de programmation, et certaines personnes préfèrent utiliser des méthodes basées sur des algorithmes plutôt que sur des sources de données réelles pour une plus grande fiabilité.

## À Voir:
Pour en savoir plus sur la génération de nombres aléatoires en Elixir, vous pouvez consulter la documentation officielle sur la fonction ```rand```: 
https://hexdocs.pm/elixir/Kernel.html#rand/0

Vous pouvez également consulter d'autres méthodes de génération de nombres aléatoires en utilisant le module ```:random```: 
https://hexdocs.pm/elixir/Random.html