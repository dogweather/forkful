---
title:                "Swift: Capitaliser une chaîne de caractères"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi capitaliser une chaîne de caractères est important en programmation Swift. La réponse est simple : cela permet de mettre en évidence certaines parties d'un texte, comme les titres ou les noms propres, et de rendre l'affichage plus cohérent.

## Comment faire

Il existe plusieurs façons de capitaliser une chaîne de caractères en Swift. Voici quelques exemples de code et leur résultat :

```Swift
let string = "hello world"
let capitalizedString = string.capitalized
print(capitalizedString) // Affiche "Hello World"

let sentence = "bonjour à tous"
let capitalizedSentence = sentence.capitalized(with: Locale(identifier: "fr")) // Prend en compte les spécificités de la langue française
print(capitalizedSentence) // Affiche "Bonjour à Tous"
```

Comme vous pouvez le voir, il suffit d'utiliser la méthode `capitalized` sur une chaîne de caractères pour obtenir sa version capitalisée. Vous pouvez également spécifier la locale pour une capitalisation plus précise selon la langue.

## Plongeon en profondeur

En plus de la méthode `capitalized`, il existe d'autres façons de capitaliser une chaîne de caractères en Swift. Par exemple, la méthode `uppercased` permet de mettre en majuscule tous les caractères, tandis que `lowercased` les mettra en minuscule.

Il est également possible d'utiliser la structure `CharacterSet` pour déterminer quels caractères doivent être capitalisés. Par exemple, le code suivant capitalisera uniquement la première lettre de chaque mot et laissera les autres en minuscule :

```Swift
let sentence = "hello world"
let firstCharacter = CharacterSet(charactersIn: " ").union(.uppercaseLetters)
let capitalizedSentence = sentence.capitalized(with: firstCharacter)
print(capitalizedSentence) // Affiche "Hello World"
```

## Voir aussi

Si vous souhaitez en savoir plus sur la manipulation de chaînes de caractères en Swift, voici quelques liens utiles :

- [Documentation officielle Apple sur les chaînes de caractères](https://developer.apple.com/documentation/foundation/string)
- [Tutoriel sur la manipulation des chaînes de caractères en Swift](https://www.hackingwithswift.com/read/0/0/strings-and-integers)
- [Vidéo explicative sur la capitalisation en Swift](https://www.youtube.com/watch?v=a6Cqg8T5ku8)