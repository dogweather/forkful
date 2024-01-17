---
title:                "Génération de nombres aléatoires"
html_title:           "Gleam: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Générer des nombres aléatoires en Gleam

## Quoi & Pourquoi?
La génération de nombres aléatoires est une fonctionnalité importante pour les programmeurs. Cela leur permet de créer des valeurs aléatoires pour différentes applications, telles que des jeux, des simulations ou des tests de logiciels. Cela peut également être utile pour améliorer la sécurité de certaines applications en générant des jetons aléatoires.

## Comment faire:
Pour générer des nombres aléatoires en Gleam, vous pouvez utiliser la fonction `gleam/random.int` qui prend en argument une valeur maximale et renvoie un nombre entier aléatoire compris entre zéro et la valeur maximale spécifiée. Voici un exemple de code:

```Gleam
import Random

Random.int(10)
// Output: 7
```

Vous pouvez également générer des nombres aléatoires en utilisant la fonction `gleam/random.float` qui renvoie un nombre à virgule aléatoire entre zéro et un. Voici un autre exemple de code:

```Gleam
import Random

Random.float()
// Output: 0.4581623889
```

## Plongée en profondeur:
La génération de nombres aléatoires est un concept qui existe depuis longtemps dans le domaine de l'informatique. Cela remonte aux années 1940, lorsque des ordinateurs ont été utilisés pour simuler des phénomènes aléatoires. Il existe également d'autres alternatives pour générer des valeurs aléatoires en Gleam, telles que l'utilisation de générateurs pseudo-aléatoires avec une graine spécifiée.

Pour implémenter la génération de nombres aléatoires en Gleam, la fonction `gleam/random.int` utilise en réalité une fonction de hachage interne pour créer un nombre aléatoire à partir de la valeur maximale spécifiée. Cela garantit une distribution uniforme des nombres générés.

## Voir aussi:
Pour en savoir plus sur la génération de nombres aléatoires en Gleam, vous pouvez consulter la documentation officielle de la bibliothèque standard de Gleam. Vous pouvez également explorer d'autres sujets liés à la programmation en Gleam en visitant leur site web et en parcourant leur communauté en ligne.