---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

---
title: Concaténation des Chaînes de Caractères en Swift

---

## Qu'est-ce que c'est et pourquoi?

La concaténation des chaînes est le processus d'assemblage de deux ou plusieurs chaînes pour former une seule. Les programmeurs le font pour manipuler des données textuelles et pour créer des messages dynamiques.

## Comment faire:

Voici quelques exemples sur comment concaténer des chaînes en Swift:

```Swift
// Déclaration de deux chaînes
var salutation = "Bonsoir, "
var nom = "Stéphanie"

// Concaténation en utilisant l'opérateur +
var message1 = salutation + nom
print(message1)  // Affiche: "Bonsoir, Stéphanie"

// Concaténation en utilisant la méthode append
salutation.append(nom)
print(salutation)  // Affiche: "Bonsoir, Stéphanie"
```

## Approfondir

1. **Contexte Historique**: Swift, sorti pour la première fois en 2014, a amélioré la manière de travailler avec les chaînes de caractères, facilitant ainsi la concaténation.
   
2. **Alternatives**: Au lieu de l'opérateur `+` ou de la méthode `append`, vous pouvez utiliser l'interpolation de chaînes pour concaténer des chaînes. Voici un exemple: 
    ```Swift
    var nom = "Stéphanie"
    var message2 = "Bonsoir, \(nom)"
    print(message2)  // Affiche: "Bonsoir, Stéphanie"
    ```  
   
3. **Détails de l'Implémentation**: La concaténation de chaînes en Swift crée une nouvelle chaîne. Les chaînes originales restent inchangées.[]

## À Voir Aussi

1. [The Swift Programming Language (Swift 5.4): Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Apple's developer documentation: String](https://developer.apple.com/documentation/swift/string)

<p></p>