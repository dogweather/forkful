---
title:                "Manipulation des nombres complexes"
aliases:
- fr/elm/working-with-complex-numbers.md
date:                  2024-01-26T04:39:22.981520-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manipulation des nombres complexes"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Les nombres complexes sont une combinaison de nombres réels et imaginaires, comme `a + bi` où `i` est la racine carrée de -1. Ils sont clés dans des domaines tels que l'ingénierie et la physique pour résoudre des problèmes que les nombres ordinaires ne peuvent pas aborder.

## Comment faire :
Elm n'a pas de support intégré pour les nombres complexes, donc vous allez créer votre propre type et fonctions. Voici une configuration rapide :

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Exemple d'utilisation :
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

somme = add a b
-- somme est { real = 4.0, imaginary = -2.0 }
```

## Exploration Approfondie
Historiquement, les nombres complexes n'ont pas toujours été acceptés. Ils sont devenus un élément transformateur au 16e siècle pour résoudre les équations cubiques. Les alternatives dans d'autres langages comme Python offrent un support intégré pour les nombres complexes avec des opérations directement disponibles. Elm nécessite une approche DIY comme vous l'avez vu. Mais vous pouvez le rendre aussi sophistiqué que nécessaire, en construisant la multiplication, la division, et d'autres opérations, en ajustant les problèmes de performance.

## Voir Aussi
- La Documentation Officielle d'Elm : https://package.elm-lang.org/ pour créer des types personnalisés et maîtriser les bases d'Elm.
- Les passionnés d'histoire des mathématiques pourraient consulter "Une Histoire Imaginaire" de Paul J. Nahin pour un voyage à travers le temps des nombres complexes.
- Plongez dans les défis de programmation orientés maths sur Project Euler (https://projecteuler.net) pour appliquer votre magie des nombres complexes.
