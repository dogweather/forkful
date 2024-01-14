---
title:                "Swift: Vérification de l'existence d'un répertoire"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de la programmation Swift pour vérifier l'existence d'un répertoire peut être très utile dans de nombreux cas, comme la gestion de fichiers et la validation de références.

## Comment Faire

Pour vérifier si un répertoire existe en Swift, vous pouvez utiliser la méthode "fileExists(atPath: String)" de la classe "FileManager". Voici un exemple de code qui vérifie l'existence d'un répertoire nommé "images" :

```Swift
if FileManager.default.fileExists(atPath: "images") {
    print("Le répertoire \"images\" existe.")
} else {
    print("Le répertoire \"images\" n'existe pas.")
}
```

La sortie de ce code sera "Le répertoire "images" existe." si le répertoire existe réellement, ou "Le répertoire "images" n'existe pas." s'il n'existe pas.

## Plongée Profonde

La méthode "fileExists(atPath: String)" de la classe "FileManager" renvoie un booléen indiquant si le répertoire existe ou non. Elle utilise le chemin absolu du répertoire en tant que paramètre, donc assurez-vous de fournir le chemin correct de votre répertoire cible.

Il est également important de noter que cette méthode vérifie uniquement l'existence d'un répertoire et non son contenu. Pour vérifier si un fichier spécifique existe à l'intérieur du répertoire, vous devrez utiliser une autre méthode, comme "fileExists(atPath: String)" ou "contentsOfDirectory(atPath: String)".

## Voir Aussi

Pour en savoir plus sur les méthodes disponibles pour gérer les fichiers et les répertoires en Swift, consultez les liens suivants :

- [Documentation officielle de la classe FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutoriel sur la gestion des fichiers en Swift](https://learnappmaking.com/read-write-files-swift-programming/)
- [Exemple de validation de l'existence d'un fichier en Swift](https://www.hackingwithswift.com/example-code/system/how-to-check-whether-a-file-exists-using-filemanager)