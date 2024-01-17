---
title:                "La génération de nombres aléatoires"
html_title:           "Bash: La génération de nombres aléatoires"
simple_title:         "La génération de nombres aléatoires"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 
Générer des nombres aléatoires, c'est créer des nombres sans logique ou relation entre eux. Les programmeurs utilisent cette technique pour ajouter de l'incertitude ou de l'imprévisibilité à leurs programmes, ce qui peut être utile pour des simulations ou des jeux.

## Comment faire: 
Voici quelques exemples de code Bash pour générer des nombres aléatoires et leur sortie:
```Bash
# Générer un nombre entier entre 1 et 10
echo $((RANDOM % 10 + 1))
# Sortie: 5

# Générer un nombre réel entre 0 et 1
echo $((RANDOM / 32768.0))
# Sortie: 0.054

# Générer une lettre aléatoire de l'alphabet
echo $((RANDOM % 26 + 97)) | awk '{printf "%c", $0}'
# Sortie: g
```

## Plongée en profondeur: 
L'utilisation de générateurs de nombres aléatoires remonte à plusieurs siècles, avec des méthodes comme le lancer de dés ou le tirage de cartes. De nos jours, les ordinateurs utilisent des algorithmes pour générer des nombres pseudo-aléatoires. Il existe également des alternatives telles que l'utilisation de matériel physique, comme des bruits thermiques, pour générer des nombres réellement aléatoires. En Bash, la commande `RANDOM` utilise un générateur pseudo-aléatoire basé sur le temps pour produire des nombres.

## Voir aussi: 
- [Guide officiel de la syntaxe Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Documentation sur la commande RANDOM](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#index-RANDOM)