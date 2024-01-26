---
title:                "Arrondir les nombres"
date:                  2024-01-26T03:44:24.243138-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Arrondir des nombres consiste à ajuster une valeur au point spécifié le plus proche—par exemple de 2,56 à 3 si nous arrondissons aux nombres entiers. Les programmeurs font cela pour simplifier ou pour répondre à certaines spécifications numériques, généralement pour éviter les nuances causées par les erreurs de précision des nombres flottants ou pour rendre la sortie conviviale pour l'utilisateur.

## Comment faire :
Dans Gleam, l'arrondi n'est pas dans la bibliothèque standard à ma dernière vérification, mais voici comment vous arrondiriez typiquement un flottant au nombre entier le plus proche en utilisant directement les fonctions Erlang :

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Résultats : 3
}
```

Résultat :
```
3
```

Vous avez une précision différente en tête ? Disons, arrondir à deux décimales ? Nous avons besoin d'un peu de mathématiques :

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Résultats : 2.57
}
```

Résultat :
```
2.57
```

## Plongée en profondeur
Historiquement, arrondir les nombres a été crucial, en particulier dans les calculs financiers et scientifiques où la précision et les normes comptent énormément. Sans arrondi, vous obtiendriez partout de longs décimales désagréables, rendant les calculs impraticables et sujets à erreurs.

Dans le monde de la programmation, différents langages offrent différentes approches, allant de fonctions intégrées à des bibliothèques mathématiques complètes. L'arrondi pourrait impliquer différentes règles – par exemple, "arrondir à la demi-supérieure" (la méthode habituelle) ou "arrondir à la demi-pair" (souvent utilisé dans les calculs financiers pour éviter les biais).

Gleam, étant un langage jeune avec des racines dans Erlang, s'appuie sur l'ensemble robuste de fonctions numériques d'Erlang. À mesure que le langage se développe, nous pourrions voir des fonctions natives introduites, réduisant le besoin d'appeler des routines externes.

## Voir aussi
- Le module :math d'Erlang pour plus de calculs numériques : https://erlang.org/doc/man/math.html
- Pour comprendre pourquoi l'arrondissement peut devenir délicat, la norme IEEE sur les nombres flottants : https://ieeexplore.ieee.org/document/8766229
- Intéressé par les mathématiques derrière cela ? Consultez "Ce que tout informaticien devrait savoir sur l'arithmétique à virgule flottante" : https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html