---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 
La lecture d'un fichier texte consiste à récupérer des données stockées sous forme de texte dans un fichier. Les programmeurs le font parce qu'il s'agit d'une façon courante de stocker et partager des informations entre différentes parties d’un programme ou différents programmes.

## Comment faire:
Voici un exemple de code Swift pour lire un fichier texte:

``` Swift
import Foundation

// Chemin du fichier
let path = "/monChemin/monFichier.txt"

do {
    // Tente de lire le contenu du fichier
    let contenu = try String(contentsOfFile: path, encoding: .utf8)
    print(contenu)

} catch {
    // capture any errors
    print("Erreur: \(error)")
}
```

La sortie de ce code sera le texte de `monFichier.txt`, à moins qu'il y ait une erreur, auquel cas l'erreur sera imprimée.

## Plongée en profondeur
Historiquement, la lecture de fichiers texte est une pratique courante dès les premiers jours de la programmation. Même aujourd'hui, elle reste une manière fiable et simple de gérer les données.

En termes d'alternatives, Swift offre des outils plus sophistiqués pour lire des fichiers, comme `InputStream` et `FileHandle`, qui vous permettent de gérer de très gros fichiers plus efficacement en lisant les données par petites portions.

En ce qui concerne les détails d'implémentation, la fonction `String(contentsOfFile:encoding:)` utilise le décodage Unicode UTF-8 par défaut, ce qui fonctionne avec la plupart des fichiers texte modernes.

## Voir aussi:
1. Documentation officielle sur Swift par Apple: https://developer.apple.com/documentation/swift
2. Guide de lecture et écriture de fichiers texte en Swift sur raywenderlich.com: https://www.raywenderlich.com/7181017-swift-standard-library-string-and-text
3. Documentation officielle sur les flux de fichiers en Swift: https://developer.apple.com/documentation/foundation/inputstream