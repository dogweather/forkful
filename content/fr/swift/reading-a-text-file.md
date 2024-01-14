---
title:                "Swift: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Swift, alors vous savez que la lecture d'un fichier texte est une tâche courante dans de nombreux projets. Cela peut être pour lire des données d'un fichier de configuration, des données d'utilisateurs à partir d'un fichier CSV, ou même simplement pour afficher le contenu d'un fichier texte à l'utilisateur. Dans cet article, nous allons explorer comment lire efficacement un fichier texte en utilisant Swift.

## Comment faire

La méthode la plus simple pour lire un fichier texte en Swift est d'utiliser la fonction `contentsOfFile` de la classe `FileManager`. Jetons un coup d'oeil à un exemple de code pour mieux comprendre:

```Swift
if let path = Bundle.main.path(forResource: "example", ofType: "txt") {
    do {
        let contents = try String(contentsOfFile: path)
        print(contents)
    } catch {
        print("Error while reading file")
    }
}
```

Dans cet exemple, nous utilisons `Bundle.main.path` pour obtenir le chemin du fichier texte dans notre bundle de l'application, puis nous utilisons `String(contentsOfFile:)` pour lire le contenu du fichier et le stocker dans une variable. Enfin, nous affichons le contenu du fichier à l'utilisateur.

Il est également possible de lire le contenu du fichier ligne par ligne en utilisant la méthode `enumerateLines` de la classe `String` :

```Swift
if let path = Bundle.main.path(forResource: "example", ofType: "txt") {
    do {
        let contents = try String(contentsOfFile: path)
        contents.enumerateLines { line, _ in
            print(line)
        }
    } catch  {
        print("Error while reading file")
    }
}
```

## Plongée en profondeur

Lors de la lecture d'un fichier texte, il est important de comprendre l'encodage de caractères utilisé dans le fichier, car cela peut avoir un impact sur la manière dont le contenu est lu. Par défaut, la méthode `contentsOfFile` utilise l'encodage de caractères système, mais vous pouvez également spécifier un autre encodage en utilisant le paramètre `usedEncoding`.

De plus, si vous avez besoin de lire une grande quantité de données à partir d'un fichier, il peut être plus efficace d'utiliser la classe `FileHandle` pour lire le contenu du fichier en utilisant un buffer.

## Voir aussi

- [La documentation de `FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- [La documentation de `String`](https://developer.apple.com/documentation/swift/string)
- [La documentation de `FileHandle`](https://developer.apple.com/documentation/foundation/filehandle)