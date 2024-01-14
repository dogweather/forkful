---
title:    "Elm: Recherche et remplacement de texte"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes en programmation, notamment lors de la manipulation de grandes quantités de données ou de la création de scripts automatisés. En utilisant Elm, un langage de programmation fonctionnel, ces tâches peuvent être réalisées de manière efficace et élégante.

## Comment Faire

Pour effectuer une recherche et un remplacement de texte en Elm, il suffit d'utiliser la fonction `String.replace` en spécifiant la chaîne de caractères à rechercher et la nouvelle valeur à insérer. Par exemple, si nous voulons remplacer "Bonjour" par "Salut" dans la chaîne de caractères "Bonjour tout le monde !", voici à quoi ressemblerait le code :

```Elm
String.replace "Bonjour" "Salut" "Bonjour tout le monde !" -- renverra "Salut tout le monde !"
```

Il est également possible de spécifier le nombre de fois que la chaîne de caractères doit être remplacée en utilisant un troisième argument. Par exemple, si nous voulons remplacer uniquement la première occurrence de "Bonjour" par "Salut", nous pouvons utiliser `String.replace "Bonjour" "Salut" "Bonjour tout le monde !" 1`.

## Plongée en Profondeur

La fonction `String.replace` utilise des expressions régulières pour effectuer la recherche et le remplacement de texte. Cela signifie que vous pouvez utiliser des caractères spéciaux pour cibler des motifs spécifiques dans une chaîne de caractères, comme par exemple `String.replace "H[eé]llo" "Bonjour" "Hello, World !"` remplacera à la fois "Hello" et "Héllo" par "Bonjour". Vous pouvez également utiliser des groupes de capture pour récupérer des parties spécifiques de la chaîne de caractères d'origine et les utiliser dans le texte de remplacement.

## Voir Aussi

- Documentation officielle sur la fonction `String.replace` en Elm : https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Tutoriel sur les expressions régulières en Elm : https://elmprogramming.com/regular-expressions.html
- Exemples pratiques de recherche et de remplacement de texte en Elm : https://github.com/elm-explorations/regex#examples