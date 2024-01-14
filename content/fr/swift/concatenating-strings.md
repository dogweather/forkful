---
title:                "Swift: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une pratique courante en programmation Swift. Elle permet de combiner plusieurs chaînes de caractères en une seule, ce qui peut être utile dans de nombreuses situations, comme par exemple pour afficher un message personnalisé à l'utilisateur ou pour construire des URL dynamiques.

## Comment faire

La concaténation de chaînes de caractères est relativement simple en Swift. Vous pouvez utiliser l'opérateur "+" pour concaténer deux chaînes de caractères ensemble. Par exemple :

```Swift
let prenom = "Marie"
let nom = "Dupont"
print("Bonjour " + prenom + " " + nom)
```

Cela affichera le message "Bonjour Marie Dupont" dans la console. Vous pouvez également utiliser la méthode "append" sur une chaîne de caractères pour ajouter du texte à la fin de cette dernière. Par exemple :

```Swift
var message = "J'aime "
message.append("le code.") 
print(message) 
```

Cela affichera "J'aime le code." dans la console.

## Plongez plus en profondeur

En plus de l'opérateur "+" et de la méthode "append", il existe d'autres méthodes pour concaténer des chaînes de caractères en Swift, telles que "join" et "insert". Vous pouvez également utiliser des interpolations de chaînes pour insérer des valeurs dynamiques dans une chaîne de caractères. Par exemple :

```Swift
let age = 25
let message = "J'ai \(age) ans."
print(message)
```

Cela affichera "J'ai 25 ans." dans la console.

Il est important de noter que la concaténation de chaînes de caractères peut être inefficace si vous l'utilisez de manière répétée sur de nombreuses chaînes de caractères. Dans ces cas-là, il peut être plus efficace d'utiliser un type de données tel que "StringBuilder" pour construire la chaîne de caractères finale pas à pas.

## Voir aussi

Pour en savoir plus sur la concaténation de chaînes de caractères en Swift, vous pouvez consulter les ressources suivantes :

- [Concaténation de chaînes de caractères dans Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID299) (Documentation officielle)
- [Swift.org](https://swift.org/) (Site officiel de Swift)