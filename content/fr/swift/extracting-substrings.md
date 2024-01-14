---
title:    "Swift: Extraction de sous-chaînes"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

La manipulation de chaînes de caractères est une tâche courante en programmation, mais parfois il est nécessaire d'extraire uniquement une partie d'une chaîne plutôt que de travailler avec la chaîne complète. C'est là qu'entre en jeu l'extraction de sous-chaînes. Avec Swift, cela peut être fait facilement et efficacement grâce à certaines méthodes et fonctions intégrées.

## Comment faire

Pour extraire une sous-chaîne d'une chaîne de caractères, il vous suffit d'utiliser la méthode `substring` en spécifiant l'index de début et l'index de fin de la sous-chaîne souhaitée. Par exemple:

```Swift
let str = "Bonjour le monde!"
let subStr = str.substring(from: 8, to: 11)
print(subStr) //sortie: "le"
```

Dans cet exemple, nous avons extrait la sous-chaîne qui va du 8ème caractère (inclus) jusqu'au 11ème caractère (exclus).

Si vous souhaitez extraire la sous-chaîne à partir d'un index donné jusqu'à la fin de la chaîne, vous pouvez utiliser la méthode `substring(from:)` en spécifiant uniquement l'index de départ.

```Swift
let subStr = str.substring(from: 8)
print(subStr) //sortie: "le monde!"
```

Si vous voulez extraire une sous-chaîne en partant de la fin de la chaîne, vous pouvez utiliser la méthode `substring(to:)` en spécifiant l'index de fin de la sous-chaîne souhaitée.

```Swift
let subStr = str.substring(to: 6)
print(subStr) //sortie: "Bonjour"
```

Enfin, si vous souhaitez extraire une sous-chaîne à partir d'un index de départ et en spécifiant le nombre de caractères à extraire, vous pouvez utiliser la méthode `substring(with:)`.

```Swift
let subStr = str.substring(with: 8..<14)
print(subStr) //sortie: "le mon"
```

## Zoom en profondeur

L'extraction de sous-chaînes en Swift est basée sur le type `Substring`, qui est une sous-classe de `String` et représente une vue sur une partie de la chaîne d'origine. Cela signifie que l'extraction d'une sous-chaîne ne nécessite pas de copier la chaîne complète, ce qui peut être utile en termes de performances et de consommation de mémoire.

Il est également important de noter que les sous-chaînes doivent être converties en chaînes de caractères complètes avant d'être utilisées en dehors du contexte dans lequel elles ont été créées. Cela peut être fait en utilisant la méthode `String()` ou en utilisant l'opérateur `+` pour concaténer la sous-chaîne à une autre chaîne.

## Voir aussi

- [Documentation officielle de Swift sur l'extraction de sous-chaînes](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID275)
- [Article Medium sur l'utilisation de sous-chaînes en Swift](https://medium.com/@abhimuralidharan/learn-swift-4-string-substring-3fe72abb75a8)