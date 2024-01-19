---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Extraire des sous-chaînes, c'est prendre une section spécifique d'une chaîne de caractères. Les programmeurs le font principalement pour analyser et manipuler les données textuelles.

## Comment Faire:

Voici un exemple rudimentaire dans Swift. Considérez une chaîne 'salut Swift':

```Swift
let salutation = "salut Swift"

let debutIndex = salutation.startIndex
let finIndex = salutation.index(debutIndex, offsetBy: 5)
let sousChaine = salutation[debutIndex..<finIndex]

print(sousChaine)
```
Cela affichera `salut`.

## Plongée Profonde

### Contexte historique :
Depuis Swift 4.0, les chaînes sont des collections, facilitant ainsi la manipulation des sous-chaînes. 

### Alternatives :
Vous pouvez également utiliser la méthode `prefix` ou `suffix` pour extraire les sous-chaînes. Par exemple:

```Swift
let prefix = salutation.prefix(5)
let suffix = salutation.suffix(5)
```
Cela retournera `salut` et `Swift` respectivement.

### Détails de mise en œuvre :
Extraction de sous-chaînes en Swift est efficace en termes de mémoire. Une sous-chaîne partage le stockage de mémoire de sa chaîne d'origine, réduisant l'empreinte mémoire nécessaire.

## Voir Aussi 

1. [Documentation officielle de Swift sur les sous-chaînes](https://developer.apple.com/documentation/swift/substring)
2. [Article de blog détaillé sur l'utilisation des sous-chaînes en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-split-a-string-into-an-array-substrings)
3. [Tutoriel vidéo sur Youtube pour extraire des sous-chaînes en Swift](https://www.youtube.com/watch?v=kP-Xn_o6f84)