---
title:    "Swift: Affichage du débogage"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous développez une application en Swift, il est important de pouvoir suivre le flux d'exécution du code afin de détecter les erreurs et de comprendre les problèmes. C'est là que l'affichage des sorties de débogage entre en jeu.

## Comment faire

Pour afficher les sorties de débogage dans votre code Swift, vous pouvez utiliser la fonction `print()` avec les valeurs que vous souhaitez afficher entre parenthèses. Par exemple :

```Swift
let nom = "Marie"
let age = 25

print("Bonjour, je m'appelle \(nom) et j'ai \(age) ans.")
```

Lorsque vous exécutez ce code, vous verrez dans la console : "Bonjour, je m'appelle Marie et j'ai 25 ans." Cela peut vous aider à suivre le déroulement de votre code et à vérifier si les variables contiennent les valeurs que vous attendiez.

## Zoom sur l'affichage des sorties de débogage

En plus d'afficher des valeurs de variables, vous pouvez également utiliser l'affichage des sorties de débogage pour afficher des messages d'erreur ou des avertissements dans votre code. Par exemple :

```Swift
let note = 17

if note < 10 {
    print("Attention, votre note est insuffisante.")
} else if note >= 10 && note < 14 {
    print("Votre note est passable.")
} else if note >= 14 && note < 18 {
    print("Félicitations, vous avez une bonne note.")
} else {
    print("Bravo, vous avez obtenu un excellent résultat !")
}
```

Ce code affiche un message différent en fonction de la valeur de la variable `note`. Si vous voulez en savoir plus sur l'affichage des sorties de débogage en Swift, vous pouvez consulter la documentation officielle d'Apple sur le sujet.

## Voir aussi

- [Documentation officielle sur l'affichage des sorties de débogage en Swift](https://developer.apple.com/documentation/swift/diagnosing_memory_issues)
- [Article Medium sur l'utilisation de l'affichage des sorties de débogage en Swift](https://medium.com/ios-os-x-development/how-to-use-nslog-like-a-pro-to-debug-your-swift-code-75167286c6e9)
- [Tutoriel vidéo sur l'affichage des sorties de débogage en Swift](https://www.youtube.com/watch?v=hxByyI9QFZ4)