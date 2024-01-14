---
title:    "Swift: Lecture d'un fichier texte"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Swift, il est fort probable que vous ayez un jour besoin de lire un fichier texte dans votre code. Que ce soit pour récupérer des données ou pour enregistrer des informations, savoir comment lire un fichier texte peut être une compétence très utile. Dans cet article, nous allons plonger dans les bases de la lecture de fichiers textes en Swift.

## Comment faire

La première étape pour lire un fichier texte en Swift est de créer un objet `FileHandle` à partir du chemin du fichier. Vous pouvez ensuite utiliser la méthode `readDataToEndOfFile()` pour lire le contenu du fichier en tant que flux d'octets. Voici un exemple de code qui lit un fichier texte appelé "monFichier.txt" :

```Swift
guard let fileURL = Bundle.main.url(forResource: "monFichier", withExtension: "txt") else {
    print("Le fichier n'existe pas")
    return
}

do {
    let fileHandle = try FileHandle(forReadingFrom: fileURL)
    let fileData = fileHandle.readDataToEndOfFile()
    let fileContents = String(data: fileData, encoding: .utf8)
    print("Contenu du fichier : \(fileContents)")
} catch {
    print("Erreur lors de la lecture du fichier : \(error)")
}
```

Lorsque vous exécutez ce code, vous devriez voir le contenu du fichier texte s'afficher dans la console.

## Plongée en profondeur

Maintenant, voyons en détail ce qui se passe dans le code ci-dessus. Tout d'abord, nous utilisons la méthode `url(forResource:withExtension:)` de la classe `Bundle` pour récupérer l'URL du fichier "monFichier.txt" dans notre application. Si le fichier n'existe pas, nous affichons un message d'erreur et sortons de la fonction.

Ensuite, nous créons un objet `FileHandle` à partir de l'URL du fichier en utilisant la méthode `forReadingFrom:`. Cette méthode peut jeter une erreur si le fichier n'est pas accessible en lecture ou n'existe pas. C'est pourquoi nous l'utilisons dans un bloc `do-try-catch`.

Une fois que nous avons notre `FileHandle`, nous pouvons utiliser la méthode `readDataToEndOfFile()` pour lire le contenu du fichier en tant que flux d'octets. Nous utilisons ensuite la méthode `String(data:encoding:)` pour convertir ces octets en une chaîne de caractères en utilisant l'encodage UTF-8. Le résultat est stocké dans la variable `fileContents`.

Enfin, nous affichons le contenu du fichier dans la console. 

## Voir aussi

- [Documentation officielle de Swift sur la lecture et l'écriture de fichiers](https://docs.swift.org/swift-book/LanguageGuide/AdvancedOperators.html)
- [Tutoriel vidéo sur la lecture et l'écriture de fichiers en Swift](https://www.youtube.com/watch?v=uNbsqCUeTLc)
- [Exemple de code pour lire et écrire des fichiers en Swift](https://www.hackingwithswift.com/read/0/32/reading-user-input)