---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi?
L'interpolation de chaîne est l'inclusion des valeurs de variables directement dans une chaîne. On le fait pour réduire la nécessité d'opérations de concaténation répétées ou excessives, améliorant ainsi la lisibilité du code.


## Comment faire:

Voici un exemple de la façon dont vous pourriez interpoler une chaîne en Haskell:

```Haskell
let name = "John"
let age = 30
print ("Salut " ++ name ++ ", tu as " ++ show age ++ " ans.")
```

Ce programme produit le résultat suivant:

```
Salut John, tu as 30 ans.
```

## Plongée en profondeur:

1. **Contexte historique:** Haskell possède des capacités d'interpolation de chaîne très primitives par rapport à d'autres langages plus modernes comme Python ou JavaScript. En raison de la nature purement fonctionnelle de Haskell, l'interpolation de chaîne doit être effectuée manuellement à l'aide de l'opérateur de concaténation "++" et de la fonction "show" pour convertir les non-chaînes en chaînes.

2. **Alternatives:** Il existe des bibliothèques Haskell comme `Text.Printf` et `Text.InterpolatedString.Perl6` qui fournissent plus de capacités d'interpolation de chaîne, semblable à ce que vous trouverez dans d'autres langages de programmation.

3. **Détails de l'implémentation:** Les opérations de concaténation de chaînes en Haskell sont généralement coûteuses en termes de performances. Si vous prévoyez d'effectuer une grande quantité d'interpolation de chaîne, il peut être préférable d'utiliser une bibliothèque dédiée ou une autre méthode de construction de chaîne plus performante.

## Voir aussi:

Pour en savoir plus sur les chaînes de caractères dans Haskell, consultez la documentation officielle:

[Haskell Wiki: Chaînes de caractères](https://wiki.haskell.org/Strings)

Pour une discussion plus approfondie sur `Text.Printf` et `Text.InterpolatedString.Perl6`, jetez un oeil ici:

[Hackage: Text.Printf](https://hackage.haskell.org/package/base/docs/Text-Printf.html)

[Hackage: Text.InterpolatedString.Perl6](https://hackage.haskell.org/package/interpolatedstring-perl6-1.0.0/docs/Text-InterpolatedString-Perl6.html)