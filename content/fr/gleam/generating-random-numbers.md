---
title:                "Gleam: La génération de nombres aléatoires"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi générer des nombres aléatoires en utilisant Gleam ?

Générer des nombres aléatoires peut être utile pour de nombreuses applications, telles que les simulations, les jeux, ou même pour tester des programmes en utilisant des données aléatoires. Avec Gleam, il est facile de générer des nombres aléatoires de manière efficace et contrôlée.

## Comment faire :

```Gleam
import gleam/random
import gleam/list

// Générer un seul nombre aléatoire entre 1 et 10
let nombre = random.int(10)

// Générer une liste de 5 nombres aléatoires entre 1 et 100
let nombres = random.ints(5, 1, 100)
```

Voici un exemple de sortie possible :

```
nombre = 7
nombres = [72, 14, 5, 99, 36]
```

Gleam offre également des fonctions pour générer des nombres aléatoires de types différents, tels que des nombres flottants, des caractères et même des données personnalisées. Consultez la documentation pour plus d'exemples et d'informations sur les différentes options disponibles.

## Plongée en profondeur :

La génération de nombres aléatoires peut sembler simple, mais il est important de comprendre les concepts sous-jacents pour obtenir des résultats fiables et prévisibles. Gleam utilise une structure appelée générateur pseudo-aléatoire pour générer des nombres aléatoires à partir d'une "graine" initiale. En utilisant une graine spécifique, il est possible de reproduire la même séquence de nombres aléatoires à chaque fois. Cela peut être utile pour le débogage et la reproductibilité.

Il est également important de noter que les nombres générés par un générateur pseudo-aléatoire ne sont pas vraiment aléatoires, mais plutôt déterministes. Cela peut sembler contre-intuitif, mais il est possible de reproduire exactement la même séquence de nombres en utilisant la même graine. Pour obtenir une véritable aléa, il est préférable d'utiliser des sources externes telles que des capteurs de température ou de bruit blanc pour initialiser le générateur.

## Voir aussi :

- [Documentation Gleam sur la génération aléatoire](https://gleam.run/lib/random)
- [Introduction à la génération de nombres aléatoires](https://www.codementor.io/@alexandarpacius/how-to-generate-random-numbers-programming-re7gut5oh)
- [Analyse de la génération de nombres aléatoires en informatique](https://www.computersciencedegreehub.com/faq/what-is-aleatory-in-computer-science/)