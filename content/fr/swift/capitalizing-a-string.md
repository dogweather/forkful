---
title:    "Swift: Capitaliser une chaîne de caractères"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Pourquoi

Vous pouvez vous demander pourquoi vous voudriez mettre une chaîne en majuscule en programmant en Swift. Une raison courante pourrait être de suivre les conventions de nommage d'une API ou d'afficher du texte en majuscule dans votre application.

# Comment Faire

Pour mettre une chaîne en majuscule en Swift, vous pouvez utiliser la méthode `uppercased()`. Voici un exemple de code pour mettre une chaîne en majuscule :

```Swift
let string = "Bonjour tout le monde!"
let uppercaseString = string.uppercased()
print(uppercaseString)
```

Résultat :

```
BONJOUR TOUT LE MONDE!
```

# Plongée Profonde

La méthode `uppercased()` utilise les règles de casse locales de votre appareil pour mettre la chaîne en majuscule. Cela signifie que si vous utilisez cette méthode sur un appareil en anglais, la chaîne sera en majuscule uniquement avec des lettres telles que "A", "B", "C", etc. Si vous utilisez cette méthode sur un appareil en français, la chaîne sera en majuscule avec des lettres telles que "À", "Ê", "Ç", etc.

Vous pouvez également utiliser la méthode `localizedUppercase` si vous voulez que la chaîne soit en majuscule en utilisant les règles de casse du pays dans lequel se trouve l'utilisateur de l'application.

# Voir Aussi

- [Documentation Apple sur la méthode `uppercased()`](https://developer.apple.com/documentation/swift/string/1787237-uppercased)
- [Documentation Apple sur la méthode `localizedUppercase()`](https://developer.apple.com/documentation/swift/string/2894563-localizeduppercase)