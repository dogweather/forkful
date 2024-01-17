---
title:                "Ecrire un fichier texte"
html_title:           "Swift: Ecrire un fichier texte"
simple_title:         "Ecrire un fichier texte"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi?

Écrire un fichier texte en programmation consiste simplement à créer un fichier qui contient du texte. Les programmeurs le font souvent pour stocker des données ou pour créer des fichiers de configuration pour leurs programmes.

## Comment faire:

Utilisez la fonction `write(to:)` sur une chaîne de caractères pour écrire votre texte dans un fichier. Par exemple:

```Swift
let texte = "C'est si facile d'écrire un fichier texte en Swift!"
do {
  try texte.write(to: URL(fileURLWithPath: "monFichier.txt"), atomically: true, encoding: .utf8)
} catch {
  print("Une erreur s'est produite lors de l'écriture du fichier: \(error.localizedDescription)")
}
```

Votre fichier texte sera créé dans le même répertoire que votre code. Voici à quoi il pourrait ressembler:

```
C'est si facile d'écrire un fichier texte en Swift!
```

## Plongée en profondeur:

Écrire des fichiers texte en programmation est une pratique courante depuis les débuts de l'informatique. Il existe de nombreuses alternatives pour stocker des données, telles que les bases de données, mais écrire des fichiers texte reste une méthode simple et efficace. En termes d'implémentation, les programmes utilisent souvent des fonctions de manipulation de fichiers pour créer, lire et écrire dans des fichiers.

## Voir aussi:

Pour en savoir plus sur l'écriture de fichiers texte en Swift, vous pouvez consulter la documentation officielle d'Apple sur le sujet: [Écrire dans un fichier texte en Swift](https://developer.apple.com/documentation/foundation/nsstring/1407721-write). Vous pouvez également consulter des tutoriels en ligne pour des exemples plus détaillés.