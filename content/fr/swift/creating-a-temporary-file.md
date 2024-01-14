---
title:    "Swift: Création d'un fichier temporaire"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire est souvent nécessaire pour stocker des données temporaires ou pour effectuer des opérations qui nécessitent l'utilisation d'un fichier. Que ce soit pour préserver l'espace de stockage de votre appareil ou pour des raisons de sécurité, créer un fichier temporaire peut être une étape importante dans la programmation en Swift.

## Comment faire

Pour créer un fichier temporaire en Swift, vous pouvez utiliser la méthode `NSTemporaryDirectory()` pour obtenir le chemin du répertoire temporaire et `URL(fileURLWithPath:)` pour créer un objet URL à partir de ce chemin. Ensuite, vous pouvez utiliser la méthode `createFile(atPath:contents:attributes:)` pour créer le fichier temporaire à cet emplacement spécifique. Voici un exemple de code :

```Swift
let tempPath = NSTemporaryDirectory()
let tempURL = URL(fileURLWithPath: tempPath).appendingPathComponent("example.txt")

do {
    try "Hello World!".write(to: tempURL, atomically: true, encoding: .utf8)
    print("Fichier temporaire créé avec succès.")
} catch {
    print(error)
}
```

En exécutant ce code, vous devriez voir le message "Fichier temporaire créé avec succès." s'afficher dans la console et un fichier "example.txt" sera créé dans votre répertoire temporaire avec le contenu "Hello World!".

## Approfondissement

Il est important de noter que les fichiers temporaires ne sont pas automatiquement supprimés une fois votre application terminée. Il est donc recommandé de les supprimer manuellement une fois que vous avez terminé de les utiliser. Vous pouvez utiliser la méthode `removeItem(at:)` pour supprimer un fichier temporaire. De plus, vous pouvez spécifier les attributs du fichier temporaire en utilisant le dictionnaire `attributes` dans la méthode `createFile(atPath:contents:attributes:)`. Cela peut inclure des informations telles que le type de fichier, la date de création ou des gestionnaires de fichiers personnalisés.

## Voir aussi

- [Documentation Apple sur les fichiers et dossiers temporaires](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/TemporaryFilesandDirectories/TemporaryFilesandDirectories.html)
- [Tutorial sur la création de fichiers temporaires en Swift](https://www.hackingwithswift.com/example-code/system/how-to-create-a-temporary-file-using-contentsoftype)
- [Exemple de gestion de fichiers dans l'application Notes d'Apple](https://developer.apple.com/documentation/uikit/view_controllers/adding_a_document_browser_to_your_app)