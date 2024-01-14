---
title:                "Swift: Impression de sortie de débogage"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de sortie de débogage peut sembler évidente à certains, mais elle peut être un outil très utile pour comprendre le comportement de votre code et trouver des erreurs dans vos programmes Swift. Cela peut être particulièrement utile lors du développement de nouvelles fonctionnalités ou de la résolution de bugs.

## Comment faire

Pour imprimer une sortie de débogage dans Swift, vous pouvez utiliser la fonction `print()`. Par exemple:

```Swift
let nombre = 10
print("Le nombre est \(nombre)") 
```

Cela produira la sortie suivante dans la console :

`Le nombre est 10`

Vous pouvez également utiliser `debugPrint()` pour obtenir une sortie de débogage plus détaillée pour les types personnalisés tels que les structures ou les classes.

```Swift
struct Personne {
    var nom: String
    var age: Int
}

let moi = Personne(nom: "Jeanne", age: 25)
debugPrint(moi) 
```

Cela produira la sortie suivante :

`Personne(nom: "Jeanne", age: 25)`

Vous pouvez également utiliser des options de formatage pour personnaliser votre sortie de débogage, telles que `separator` et `terminator`. Par exemple :

```Swift
let nombre1 = 5
let nombre2 = 7
print(nombre1, nombre2, separator: " + ", terminator: " = ")
print(nombre1 + nombre2) 
```

Cela produira la sortie suivante :

`5 + 7 = 12`

## Plongée en profondeur

L'impression de sortie de débogage peut également être utile à des fins de débogage avancées. Par exemple, vous pouvez utiliser `#function` pour imprimer le nom de la fonction dans laquelle la sortie est appelée.

```Swift
func imprimerFonction() {
    print("Fonction appelée: \(#function)")
}

imprimerFonction() 
```

Cela produira la sortie suivante :

`Fonction appelée: imprimerFonction()`

Vous pouvez également utiliser `#line`, `#file` et `#column` pour imprimer des informations sur la ligne, le fichier et la colonne où la sortie est appelée.

## Voir aussi

- [Documentation Apple sur l'impression de sortie de débogage](https://developer.apple.com/documentation/swift/debugging)
- [Article sur l'utilisation de l'impression de sortie de débogage en Swift](https://www.hackingwithswift.com/sixty/12/6/debugging-print)
- [Guide complet sur le débogage en Swift](https://www.raywenderlich.com/4448-swift-debugging-tips-and-tricks)