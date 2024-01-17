---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Swift: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

Vérifier si un répertoire existe est simplement le fait de vérifier si un dossier spécifié existe ou non sur votre ordinateur. Les programmeurs font généralement cela pour s'assurer qu'un répertoire nécessaire pour leur code est présent avant de poursuivre l'exécution du programme.

# Comment faire :

Pour vérifier si un répertoire existe en utilisant Swift, il existe plusieurs options. Vous pouvez utiliser la fonction `fileExists()` de la classe FileManager ou la méthode `fileExists(atPath:)` pour vérifier si un chemin de fichier existe. Voici un exemple de code :

```Swift
let fileManager = FileManager.default
let path = "/Users/username/Documents/test"
if fileManager.fileExists(atPath: path) {
    print("Le répertoire existe!")
} else {
    print("Le répertoire n'existe pas!")
}
```
Lorsque vous exécutez ce code, si le répertoire "test" existe, vous verrez la phrase "Le répertoire existe!" s'afficher dans la console.

# Plongée en profondeur :

Vérifier si un répertoire existe est une tâche courante pour les programmeurs, en particulier lorsqu'ils travaillent avec des fichiers et des dossiers sur leur ordinateur. Avant Swift 3, la méthode `fileExists(atPath:)` était utilisée pour vérifier si un chemin de fichier existait. Avec la sortie de Swift 3, la fonction `fileExists()` est apparue et est devenue la méthode privilégiée pour vérifier l'existence d'un répertoire ou d'un fichier.

En ce qui concerne les alternatives, vous pouvez également utiliser la méthode `contentsOfDirectory(atPath:)` pour obtenir une liste des fichiers et des dossiers présents dans un répertoire, puis vérifier si le répertoire spécifié est présent dans cette liste. Cela peut être utile si vous avez besoin de travailler avec tous les fichiers d'un répertoire donné.

# Voir aussi :

Vous pouvez en apprendre davantage sur les fonctions liées à la gestion des fichiers et des répertoires en consultant la documentation officielle de Swift sur FileManager : https://developer.apple.com/documentation/foundation/filemanager.

Vous pouvez également consulter des forums de développeurs tels que Stack Overflow pour trouver des réponses ou des exemples de code pour résoudre des problèmes spécifiques liés à la vérification de l'existence de répertoires.