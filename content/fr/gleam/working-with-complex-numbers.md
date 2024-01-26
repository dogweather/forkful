---
title:                "Manipulation des nombres complexes"
date:                  2024-01-26T04:40:41.003218-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manipulation des nombres complexes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres complexes ont une partie réelle et une partie imaginaire (`a + bi`). Ils sont utiles dans divers domaines tels que le génie électrique et l'informatique quantique. Les programmeurs les utilisent pour modéliser des équations qui ne peuvent pas être résolues en utilisant seulement des nombres réels.

## Comment faire :
Gleam n'a pas de support natif pour les nombres complexes. Vous devriez généralement créer le vôtre ou trouver une bibliothèque. Voici un rapide exemple de la manière dont vous pourriez implémenter les opérations de base :

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let somme = add(num1, num2)
  let produit = multiply(num1, num2)

  somme // Complex(4.0, 6.0)
  produit // Complex(-5.0, 10.0)
}
```

## Plongée en profondeur

Les nombres complexes ont été documentés de manière plus formelle pour la première fois par Gerolamo Cardano au 16e siècle. Ils sont une extension naturelle des nombres réels. Cependant, dans un langage jeune comme Gleam, qui privilégie la performance et la sécurité des types, de telles fonctionnalités sont à peine esquissées (ou vous devez le faire vous-même).

Dans certains autres langages, comme Python, les nombres complexes sont intégrés (`3+4j`), ce qui facilite la vie. Dans Rust ou Haskell, vous avez des bibliothèques qui offrent des fonctionnalités avancées prêtes à l'emploi.

L'approche de Gleam signifie que vous devez gérer tous les aspects : arithmétique, coordonnées polaires, formes exponentielles, etc. La mise en œuvre d'opérations efficaces et précises implique une programmation soignée, compte tenu de la façon dont le comportement des nombres à virgule flottante peut affecter vos résultats.

N'oubliez pas de tester minutieusement, surtout les cas limites ! La gestion de l'infini complexe et des valeurs NaN (non un nombre) peut vous poser problème si vous n'êtes pas prudent.

## Voir également
Pour plus de pépites, voici où vous pouvez plonger :
- [Docs Officielles de Gleam](https://gleam.run/documentation/)
- Explorez les bibliothèques d'autres langages pour vous inspirer, comme le [num-complex](https://crates.io/crates/num-complex) de Rust ou le [module cmath](https://docs.python.org/3/library/cmath.html) de Python.