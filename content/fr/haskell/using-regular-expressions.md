---
title:                "Haskell: Utiliser les expressions régulières"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant pour vérifier et manipuler des chaînes de caractères. Elles vous permettent d'effectuer des recherches complexes et de remplacer du texte en utilisant des modèles prédéfinis. En utilisant des expressions régulières, vous pouvez gagner du temps et simplifier vos tâches de programmation en travaillant avec des données textuelles.

## Comment faire

Pour utiliser des expressions régulières en Haskell, vous devez d'abord importer le module "Text.Regex.Posix". Ensuite, vous pouvez utiliser la fonction "Regex.Posix.mkRegex" pour créer un objet de type "Regex" à partir de votre modèle. Par exemple, si nous voulons vérifier si une chaîne contient un nombre à deux chiffres, nous pouvons utiliser le modèle "[0-9]{2}", qui signifie qu'il doit y avoir exactement deux chiffres entre 0 et 9. Voici un exemple de code :

```Haskell
import Text.Regex.Posix

let regex = mkRegex "[0-9]{2}" -- création de l'objet Regex
let str = "Je suis né en 1992" -- chaîne à vérifier
match = str =~ regex :: Bool -- application du modèle à la chaîne
print match -- affiche True car la chaîne contient un nombre à deux chiffres
```

Vous pouvez également utiliser la fonction "Regex.Posix.matchRegex" pour extraire les parties de la chaîne qui correspondent à votre modèle. Par exemple, si nous voulons extraire les mots qui contiennent la lettre "e", nous pouvons utiliser le modèle "[a-z]*e[a-z]*", qui signifie qu'il y a un nombre indéfini de lettres avant et après la lettre "e". Voici un exemple de code :

```Haskell
import Text.Regex.Posix

let regex = mkRegex "[a-z]*e[a-z]*" -- création de l'objet Regex
let str = "Je suis un programmeur Haskell" -- chaîne à vérifier
matches = str =~ regex :: Maybe [String] -- application du modèle à la chaîne
print matches -- affiche Just ["Je", "suis", "programmeur"]
```

## Plongée en profondeur

Les expressions régulières en Haskell offrent également des fonctionnalités avancées telles que la recherche et le remplacement dans des fichiers et la correspondance de motifs avec des fonctions pour effectuer des opérations spécifiques. Pour en savoir plus sur ces fonctionnalités, consultez la documentation officielle de Haskell sur les expressions régulières.

## Voir aussi

- [Documentation officielle de Haskell](https://www.haskell.org/hoogle/?hoogle=regex) 
- [Tutoriel sur les expressions régulières en Haskell](https://wiki.haskell.org/Regular_expressions)
- [Exemples de cas pratiques avec les expressions régulières en Haskell](http://book.realworldhaskell.org/read/regular-expressions.html)