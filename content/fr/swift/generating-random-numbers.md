---
title:    "Swift: Génération de nombres aléatoires"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Pourquoi
Lorsque nous programmons en Swift, il y a des moments où nous avons besoin de générer des nombres aléatoires. Cela peut être pour créer des jeux, des simulations ou même pour tester notre code. Heureusement, Swift a une fonction intégrée pour générer des nombres aléatoires, ce qui rend la tâche beaucoup plus facile.

## Comment faire
Pour générer des nombres aléatoires en Swift, nous utiliserons la fonction `random (in:range)`. Voici un exemple de code qui génère un nombre aléatoire entre 0 et 10 :

```Swift
let randomNumber = Int.random (in: 0...10)
print (randomNumber)
```

Le code ci-dessus déclare une constante appelée `randomNumber` qui est définie comme un entier (`Int`). Nous utilisons ensuite la fonction `random (in:range)` en spécifiant la plage de nombres que nous voulons, dans ce cas de 0 à 10. Enfin, le nombre aléatoire est imprimé à l'écran.

Si nous voulons générer un nombre aléatoire à virgule flottante, nous pouvons utiliser la fonction `random (in:range)` en remplaçant `Int` par `Double` ou `Float`.

```Swift
let randomDouble = Double.random (in: 0...10)
let randomFloat = Float.random (in: 0...10)
```

Nous pouvons également utiliser la fonction `random (in:range)` avec des types de données comme `String` ou même `Bool`.

## Plongée en profondeur
Maintenant, si vous êtes curieux de savoir ce qui se passe en coulisses lors de la génération de nombres aléatoires en Swift, voici quelques informations supplémentaires. La fonction `random (in:range)` utilise le générateur de nombres aléatoires de Swift, qui est basé sur l'algorithme Mersenne Twister. Cela signifie que les nombres produits sont assez aléatoires et sans biais.

Il est également important de noter que chaque fois que nous exécutons le code, nous obtenons un nombre différent car le générateur de nombres aléatoires utilise une "graine" qui est basée sur la date et l'heure au moment de l'exécution du code.

## À voir aussi
- [Documentation officielle sur la fonction random ()](https://developer.apple.com/documentation/swift/int/3010689-random)
- [Article sur la génération de nombres aléatoires en Swift](https://www.hackingwithswift.com/example-code/arrays/how-to-shuffle-an-array-in-ios-and-swift)
- [Guide complet sur la génération de nombres aléatoires en Swift](https://www.swiftbysundell.com/posts/random-numbers-in-swift)