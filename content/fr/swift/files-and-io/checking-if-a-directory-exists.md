---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:49.185618-07:00
description: "V\xE9rifier si un r\xE9pertoire existe dans le syst\xE8me de fichiers\
  \ est essentiel pour g\xE9rer les structures de fichiers depuis vos applications\
  \ Swift. Cette\u2026"
lastmod: 2024-02-19 22:05:16.891740
model: gpt-4-0125-preview
summary: "V\xE9rifier si un r\xE9pertoire existe dans le syst\xE8me de fichiers est\
  \ essentiel pour g\xE9rer les structures de fichiers depuis vos applications Swift.\
  \ Cette\u2026"
title: "V\xE9rifier si un r\xE9pertoire existe"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Vérifier si un répertoire existe dans le système de fichiers est essentiel pour gérer les structures de fichiers depuis vos applications Swift. Cette tâche permet aux développeurs de vérifier la présence de répertoires avant de tenter de les lire ou d'écrire dedans, évitant ainsi d'éventuelles erreurs d'exécution.

## Comment faire :

Le framework Foundation de Swift fournit la classe `FileManager`, qui dispose de méthodes pour gérer le système de fichiers. Vous pouvez utiliser `FileManager` pour vérifier si un répertoire existe. Voici un extrait sur comment faire cela :

```swift
import Foundation

let fileManager = FileManager.default
let path = "/chemin/vers/votre/repertoire"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Le répertoire existe")
} else {
    print("Le répertoire n'existe pas")
}
```

Cependant, cela vérifie à la fois les fichiers et les répertoires. Si vous souhaitez spécifiquement vérifier qu'un répertoire existe, vous devez passer un pointeur vers une valeur booléenne dans `isDirectory` :

```swift
import Foundation

let fileManager = FileManager.default
let path = "/chemin/vers/votre/repertoire"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Le répertoire existe")
} else {
    print("Le répertoire n'existe pas")
}
```

### Utiliser une bibliothèque tierce

À l'heure actuelle, vérifier l'existence d'un répertoire en Swift ne nécessite généralement pas de bibliothèques tierces en raison de la robustesse de la classe `FileManager`. Cependant, pour des manipulations et vérifications de fichiers plus complexes, des bibliothèques comme **Files** de John Sundell fournissent une API plus conviviale pour Swift.

Voici comment vous pourriez l'utiliser :

D'abord, ajoutez Files à votre projet via Swift Package Manager.

Ensuite, vous pouvez vérifier l'existence d'un répertoire comme ceci :

```swift
import Files

do {
    _ = try Folder(path: "/chemin/vers/votre/repertoire")
    print("Le répertoire existe")
} catch {
    print("Le répertoire n'existe pas")
}
```

Note : Comme les bibliothèques tierces peuvent changer, référez-vous toujours à la documentation la plus récente pour l'utilisation et les meilleures pratiques.
