---
title:                "Créer un fichier temporaire"
html_title:           "Swift: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Créer un fichier temporaire est une tâche courante pour les programmeurs en Swift. Il s'agit de générer un fichier qui n'est pas nécessaire de conserver après l'exécution du programme. Les programmeurs utilisent cette technique pour stocker temporairement des données ou pour tester des fonctionnalités sans altérer le code source.

## Comment faire:

Voici comment créer un fichier temporaire en Swift:

```Swift 
let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent("example.txt")

do {
    try "Hello World!".write(to: tempFile, atomically: true, encoding: .utf8)
    print("Fichier temporaire créé: \(tempFile.path)")
} catch {
    print("Erreur: \(error)")
}
```

Résultat:

```
Fichier temporaire créé: /var/folders/q5/6yvdtw910hdgdy2vm7bltx4h0000gn/T/example.txt
```

L'exemple ci-dessus utilise la méthode `write` pour écrire une chaîne de caractères dans le fichier temporaire. Vous pouvez également utiliser `createFile` pour créer un fichier vide ou `contents` pour écrire des données binaires.

## Profondeur de plongée:

Les fichiers temporaires sont couramment utilisés pour stocker des données dans des applications qui utilisent une architecture basée sur la sécurité. Ils sont également utiles pour générer des fichiers uniquement durant l'exécution du programme. Il existe des alternatives à la création de fichiers temporaires, telles que l'utilisation de variables ou de structures existantes en mémoire, mais elles peuvent ne pas convenir à tous les cas d'utilisation.

En ce qui concerne la mise en œuvre, la méthode `write` crée le fichier temporaire de façon atomique, ce qui signifie qu'elle le crée de manière sûre pour le système de fichiers et ne le supprime pas si une erreur se produit pendant l'écriture. Assurez-vous toujours de supprimer manuellement le fichier après utilisation pour éviter tout encombrement inutile.

## À voir aussi:

Pour en savoir plus sur la manipulation des fichiers en Swift, consultez la documentation officielle ici: https://developer.apple.com/documentation/foundation/filemanager

D'autres langues de programmation ont également des méthodes pour créer des fichiers temporaires, telles que `tempfile` en Python ou `TempFile` en C++. Vous pouvez explorer ces options si vous êtes intéressé par la comparaison entre différentes langues.