---
title:    "Elm: Concaténer des chaînes de caractères"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi
Lorsque l'on programme en Elm, il est parfois nécessaire de combiner plusieurs chaînes de caractères en une seule. La concaténation de chaînes peut sembler un concept simple, mais elle peut s'avérer très utile dans certaines situations.

## Comment faire
La concaténation de chaînes en Elm se fait à l'aide de l'opérateur `++`. Voici un exemple de code utilisant cet opérateur :

```Elm
nom = "Jean"
prenom = "Dupont"
nomComplet = prenom ++ " " ++ nom

-- Résultat : nomComplet = "Jean Dupont"
```

On peut également concaténer des chaînes avec des nombres en utilisant la fonction `toString` :

```Elm
nb1 = 1
nb2 = 2
resultat = "Le résultat est : " ++ (toString (nb1 + nb2))

-- Résultat : resultat = "Le résultat est : 3"
```

Il est important de noter que l'ordre des opérations est important lors de la concaténation de plusieurs éléments. Par exemple, si on veut inclure une chaîne de caractères entre deux autres, il faudra la placer entre des parenthèses pour éviter tout problème :

```Elm
nom = "Jean"
prenom = "Dupont"
nomComplet = "Bonjour, je m'appelle " ++ (prenom ++ " " ++ nom) ++ ", ravie de vous rencontrer !"

-- Résultat : nomComplet = "Bonjour, je m'appelle Jean Dupont, ravie de vous rencontrer !"
```

## Plongée en profondeur
La concaténation de chaînes en Elm se fait en temps constant, c'est-à-dire que peu importe la longueur des chaînes à concaténer, le temps de traitement sera toujours le même. Cela peut avoir un impact positif sur les performances de votre application.

Il est également possible de concaténer des listes de chaînes en utilisant la fonction `List.concat` :

```Elm
listes = [["Bonjour", "hey"], ["c'est", "ça"], ["moi", "toi"]]
resultat = List.concat listes

-- Résultat : resultat = ["Bonjour", "hey", "c'est", "ça", "moi", "toi"]
```

Il existe également d'autres fonctions pratiques pour travailler avec des chaînes de caractères en Elm, telles que `String.split` pour diviser une chaîne en une liste de sous-chaînes, ou `String.join` pour joindre une liste de chaînes en une seule.

## Voir aussi
- [Documentation officielle d'Elm sur la concaténation de chaînes](https://elm-lang.org/docs/string#append)
- [Le guide complet d'Elm sur les chaînes de caractères](https://guide.elm-lang.org/interop/strings.html)