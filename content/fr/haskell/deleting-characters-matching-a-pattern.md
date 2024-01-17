---
title:                "Supprimer les caractères correspondant à un motif"
html_title:           "Haskell: Supprimer les caractères correspondant à un motif"
simple_title:         "Supprimer les caractères correspondant à un motif"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La suppression de caractères correspondant à un modèle est un processus couramment utilisé par les programmeurs pour supprimer des éléments spécifiques d'une chaîne de caractères ou d'une liste. Cela peut être utile lors de la manipulation de données ou de la validation des entrées utilisateur.

## Comment faire:

Voici un exemple de code Haskell montrant comment supprimer tous les espaces d'une chaîne de caractères:

```Haskell
deleteSpaces :: String -> String
deleteSpaces str = filter (/= ' ') str

main :: IO ()
main = do
    let str = "Haskell est un langage de programmation fonctionnel"
    let result = deleteSpaces str
    putStrLn result
```

Résultat:

```
Haskellestunlangagedeprogrammationfonctionnel
```

## Un aperçu plus approfondi:

### Contexte historique:
La suppression de caractères correspondant à un modèle a été largement popularisée dans les langages de programmation fonctionnels tels que Haskell. Cependant, cette technique a également été utilisée dans des langages impératifs tels que C ou Java.

### Alternatives:
Il existe différentes approches pour supprimer des caractères en fonction d'un modèle, telles que l'utilisation d'expressions régulières ou de fonctions de manipulation de chaînes de caractères. Cependant, la méthode de suppression de caractères correspondant à un modèle en utilisant la fonction ```filter``` de Haskell est souvent la plus simple et la plus efficace.

### Détails d'implémentation:
La fonction ```filter``` de Haskell prend en entrée une fonction de prédicat et une liste, et retourne une nouvelle liste contenant uniquement les éléments pour lesquels la fonction de prédicat renvoie ```True```. Dans le cas de la suppression de caractères correspondant à un modèle, la fonction de prédicat peut être définie comme ```(/= ' ')```, ce qui signifie que tous les caractères qui ne sont pas des espaces seront conservés.

## Voir aussi:

- [Documentation officielle de la fonction ```filter``` de Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:filter)
- [Un tutoriel sur la manipulation de chaînes de caractères en Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Un guide sur l'utilisation d'expressions régulières en Haskell](https://www.fpcomplete.com/blog/2017/09/definitive-guide-to-conduit)