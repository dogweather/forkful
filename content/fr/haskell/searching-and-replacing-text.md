---
title:    "Haskell: Recherche et remplacement de texte"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

La recherche et le remplacement de texte sont des tâches courantes en programmation. Que ce soit pour corriger des erreurs dans un code existant ou pour effectuer des modifications massives sur un grand nombre de fichiers, cette fonctionnalité est essentielle pour tout développeur.

# Comment faire

Il existe différentes façons de rechercher et de remplacer du texte en Haskell, mais nous allons nous concentrer sur l'utilisation de la fonction `substitute` du module `Text.Regex.TDFA`.

```Haskell
import Text.Regex.TDFA

replaceText :: String -> String -> String -> String
replaceText regex replacement input =
    let replaced = substitute (makeRegex regex) replacement input
    in case replaced of
        Just result -> result
        Nothing -> input
```

Dans l'exemple ci-dessus, nous utilisons la fonction `substitute` pour rechercher et remplacer du texte dans une chaîne de caractères donnée. Nous utilisons également la fonction `makeRegex` pour créer une expression régulière à partir d'une chaîne de caractères donnée. Enfin, nous utilisons le mot-clé `case` pour gérer le cas où le remplacement ne peut pas être effectué correctement.

Voici un exemple de sortie pour la chaîne de caractères `"Bonjour, le monde !"` avec regex `monde` et remplacement `univers` :

```
Bonjour, le univers !
```

# Plongée en profondeur

En plus de la fonction `substitute`, le module `Text.Regex.TDFA` offre également d'autres fonctions utiles pour effectuer des recherches et des remplacements. Par exemple, la fonction `substituteAll` permet d'effectuer des remplacements multiples dans une chaîne de caractères.

De plus, le module `Text.Regex.TDFA` utilise des expressions régulières de type `ByteString` qui offrent de meilleures performances que les expressions régulières de type `String`.

Il est également possible d'utiliser des expressions régulières avancées telles que les groupes de capture et les modificateurs pour effectuer des recherches et des remplacements plus complexes.

# Voir aussi

- [Documentation du module `Text.Regex.TDFA`](https://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html)
- [Guide de référence sur les expressions régulières en Haskell](https://wiki.haskell.org/Regular_expressions)