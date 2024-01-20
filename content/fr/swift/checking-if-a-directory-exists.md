---
title:                "Vérifier si un répertoire existe"
html_title:           "Lua: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Vérifier si un répertoire existe en Swift

## Qu'est-ce que c'est et Pourquoi?

Vérifier si un répertoire existe est une tâche courante qui permet de s'assurer que nos programmes sont stables et sécurisés. C'est une pratique essentielle pour prévenir des erreurs lors de l'accès à des fichiers.

## Comment faire:

En Swift, vous pouvez utiliser la méthode `fileExists(atPath:)` sur une instance de `FileManager` pour vérifier si un répertoire spécifique existe. Voici un exemple:

```Swift
import Foundation 

let fileManager = FileManager.default
let directoryPath = "/path/to/directory"

if fileManager.fileExists(atPath: directoryPath) {
    print("Le répertoire existe.")
} else {
    print("Le répertoire n'existe pas.")
}
```

Si le répertoire existe, ce qui se produirait:

```
Le répertoire existe.
```

Et si le répertoire n'existe pas, on aura:

```
Le répertoire n'existe pas.
```

## Exploration en profondeur

Historiquement, les programmeurs vérifient s'ils un répertoire existe pour éviter les erreurs d'exécution et sécuriser le processus de lecture / écriture de fichiers. Swift facilite ces vérifications grâce à la classe `FileManager`.

Comme alternative, vous pouvez utiliser la méthode `attributesOfItemAtPath(_:)` qui renvoie un dictionnaire contenant les attributs du fichier ou du répertoire à l'emplacement donné. Cependant, cette méthode peut renvoyer une erreur si le chemin d'accès ne mène pas à un fichier ou à un répertoire existant, d'où l'intérêt de `fileExists(atPath:)`.

L'implémentation de `fileExists(atPath:)` en Swift est simplement une liaison à une fonction C dans le sous-système de fichiers UNIX du système d'exploitation, ce qui signifie que c'est une vérification efficace et performante.

## Voir aussi

Pour de plus amples informations, vous pouvez consulter les ressources suivantes:

- La documentation officielle de Swift sur [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- Un exemple détaillé sur [how to check if a file exists or not, using Swift](https://www.hackingwithswift.com/example-code/system/how-to-check-whether-a-file-exists)