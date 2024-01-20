---
title:                "Mettre en majuscule une chaîne"
html_title:           "Swift: Mettre en majuscule une chaîne"
simple_title:         "Mettre en majuscule une chaîne"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Mettre en majuscule une chaîne de caractères signifie convertir la première lettre de chaque mot en majuscule. Les programmeurs utilisent souvent cette méthode pour rendre les sorties de texte plus lisibles et plus professionnelles.

## Comment faire :

Voici un simple exemple de mise en majuscule d'une chaîne en Swift :

```Swift
let phrase = "bonjour le monde"
let capitalizedPhrase = phrase.capitalized
print(capitalizedPhrase)
```
Cela produira l'affichage :

```Swift
"Bonnejour Le Monde"
```

## Plongée Profonde 

Mettre en majuscule une chaîne était une pratique courante dans les journaux imprimés pour mettre en évidence les titres. En Swift, l'opération est simple avec la propriété `capitalized` de la classe `String`.

Alternativement, vous pouvez utiliser la fonction `uppercased()` pour convertir toutes les lettres en majuscules, pas seulement la première lettre de chaque mot.

```Swift
let phrase = "bonjour le monde"
let upperCasedPhrase = phrase.uppercased()
print(upperCasedPhrase)
```

Cela produira l'affichage :

```Swift
"BONJOUR LE MONDE"
```
Sachez que ces méthodes ne modifient pas la chaîne originale car les chaînes en Swift sont immuables.

## Voir Aussi 

Pour en savoir plus sur la manipulation des chaînes en Swift, consultez le [guide officiel de Apple](https://developer.apple.com/documentation/swift/string). 
Les [documents de la Fondation Swift](https://developer.apple.com/documentation/foundation/nsstring) expliquent également de nombreux détails d'implémentation supplémentaires.