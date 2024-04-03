---
date: 2024-01-26 00:57:58.109377-07:00
description: "G\xE9rer les erreurs en Swift signifie anticiper et r\xE9pondre aux\
  \ probl\xE8mes qui surviennent lorsque votre code est ex\xE9cut\xE9. Nous le faisons\
  \ pour contr\xF4ler le\u2026"
lastmod: '2024-03-13T22:44:58.230554-06:00'
model: gpt-4-1106-preview
summary: "G\xE9rer les erreurs en Swift signifie anticiper et r\xE9pondre aux probl\xE8\
  mes qui surviennent lorsque votre code est ex\xE9cut\xE9."
title: Gestion des erreurs
weight: 16
---

## Quoi & Pourquoi ?
Gérer les erreurs en Swift signifie anticiper et répondre aux problèmes qui surviennent lorsque votre code est exécuté. Nous le faisons pour contrôler le chaos—empêcher les applications de planter et offrir une expérience fluide à l'utilisateur.

## Comment faire :
Swift utilise la gestion des erreurs avec les blocs `do`, `try`, et `catch`. Jetons un coup d'œil :

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Imaginons que nous avons une logique ici pour vérifier si un fichier existe et si nous avons la permission de le lire
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Le contenu du fichier va ici"
}

do {
    let fileContent = try readFile(atPath: "/chemin/vers/le/fichier")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Oups ! Fichier non trouvé.")
} catch FileError.noPermission {
    print("Ah ! Pas la permission de lire le fichier.")
} catch {
    print("Une erreur inconnue s'est produite.")
}

```

Exemple de sortie :

```
Oups ! Fichier non trouvé.
```

## Approfondissement
La gestion des erreurs n'a pas toujours été aussi élégante qu'elle l'est maintenant. En Objective-C, vous deviez gérer des pointeurs vers des objets NSError, ce qui était plutôt lourd. Maintenant, nous avons un système plus élégant avec les énumérations Swift et le protocole `Error`.

Le `throw` de Swift nous permet de signaler que quelque chose a déraillé. Les blocs `do` agissent comme des royaumes conscients des erreurs, `try` préfixe les appels risqués, et `catch` gère les choses si elles tournent mal.

Les optionnels sont une alternative pour les situations qui ne sont pas tout à fait de "statut d'erreur", mais qui pourraient quand même n'avoir "aucun résultat". Ils sont un peu comme les variables de Schrödinger—elles ont une valeur ou elles n'en ont pas.

Pour une réelle profondeur, découvrez les types `Result`, qui sont des hybrides astucieux entre des modèles de retour habituels et des motifs d'erreur.

## Voir Aussi
- Guide Officiel de Gestion des Erreurs Swift : [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Meilleures Pratiques de Gestion des Erreurs Swift : [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Gestion Avancée des Erreurs en Swift : [Article Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
