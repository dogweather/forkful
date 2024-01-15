---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Swift: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent important de manipuler des chaînes de caractères, et parfois ces chaînes doivent être capitalisées pour différentes raisons. Dans cet article, nous allons voir pourquoi et comment capitaliser une chaîne en utilisant le langage de programmation Swift.

## Comment faire

Pour capitaliser une chaîne en Swift, nous pouvons utiliser la méthode `capitalized` sur la chaîne elle-même. Voici un exemple de code:

```Swift
let str = "bonjour tout le monde"
let strCapitalized = str.capitalized
print(strCapitalized)
```

Lorsque nous exécutons ce code, nous obtenons le résultat suivant:

```
Bonjour Tout Le Monde
```

Comme vous pouvez le voir, la méthode `capitalized` a transformé la première lettre de chaque mot en majuscule, mais a laissé les autres lettres en minuscule.

Il est également possible d'utiliser la méthode `capitalizedFirstLetter`, qui capitalise seulement la première lettre d'une chaîne donnée.

```Swift
let str = "au revoir"
let strCapitalized = str.capitalizedFirstLetter
print(strCapitalized)
```

Le résultat sera:

```
Au revoir
```

Il existe également d'autres méthodes pour capitaliser une chaîne en fonction de différentes règles grammaticales, telles que `uppercased` et `localizedCapitalized`. Vous pouvez les explorer et les utiliser en fonction de vos besoins.

## Plongée en profondeur

Il est important de noter que la méthode `capitalized` utilise la règle Unicode pour la capitalisation des caractères. Cela signifie que les lettres qui ne sont pas équivalentes en majuscules et minuscules dans certaines langues ne seront pas capitalisées correctement.

De plus, il existe également des règles de capitalisation spécifiques pour certaines langues, comme la langue française où les adjectifs de nationalité doivent être écrits avec une majuscule. Dans ce cas, il est recommandé d'utiliser la méthode `localizedCapitalized` pour assurer une capitalisation appropriée en fonction de la langue de l'utilisateur.

## Voir aussi

- [Documentation officielle de la méthode capitalized en Swift](https://developer.apple.com/documentation/swift/string/2995563-capitalized)
- [Article sur la capitalisation en Swift sur le blog Medium](https://medium.com/swift-programming/string-capitalization-in-swift-47b3f695c829)