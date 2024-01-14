---
title:                "Swift: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi écrire vers l'erreur standard en Swift
Lorsque vous êtes en train de programmer en Swift, il peut souvent être utile de voir les messages d'erreur de votre code directement dans votre environnement de développement. Cela peut vous aider à repérer les erreurs et à les corriger plus rapidement. C'est là que l'écriture vers l'erreur standard entre en jeu.

## Comment faire
Pour écrire vers l'erreur standard en Swift, vous pouvez utiliser la fonction `fwrite()` avec `stderr` comme paramètre. Voici un exemple de code qui montre comment utiliser cette fonction :

```Swift
import Foundation

// Définition d'une fonction pour écrire vers l'erreur standard
func writeError(message: String) {
    let error = "Erreur : " + message + "\n"
    guard let errorData = error.data(using: .utf8) else {
        print("Impossible de convertir le message en données.")
        return
    }
    // Écriture vers l'erreur standard
    fwrite(errorData, 1, errorData.count, stderr)
}

// Exemple d'utilisation de la fonction
writeError(message: "Valeur invalide pour la variable.")
```

Lorsque vous exécutez ce code, vous verrez le message d'erreur s'afficher dans votre console, en rouge et précédé de la mention "Erreur : ".

## Plongeons plus profondément
En écrivant vers l'erreur standard en Swift, vous pouvez également utiliser la fonction `fputs()` pour écrire une chaîne de caractères directement vers l'erreur standard. Voici un exemple :

```Swift
// Écriture d'une chaîne de caractères vers l'erreur standard
fputs("Erreur : Valeur invalide pour la variable.\n", stderr)
```

De plus, vous pouvez également utiliser `NSFileHandle` pour écrire vers l'erreur standard en utilisant la méthode `writeData()`.

Dans certains cas, vous voudrez peut-être également afficher des informations de débogage dans l'erreur standard. Pour cela, vous pouvez utiliser la fonction `debugPrint()` qui affiche les valeurs des variables de manière lisible pour les humains.

## See Also
- [Documentation Apple pour la fonction `fwrite()`](https://developer.apple.com/documentation/foundation/filehandle/1410972-fwrite?language=swift)
- [Article de référence sur l'utilisation de `fprintf()` et `stderr`](https://www.dummies.com/programming/c/how-to-display-information-in-the-standard-error-file-stderr/)
- [Documentation Apple pour la fonction `fputs()`](https://developer.apple.com/documentation/foundation/filehandle/1409761-fputs?language=swift)