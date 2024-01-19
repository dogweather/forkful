---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Kotlin: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
En programmation, vérifier si un répertoire existe est une pratique courante. Il s'agit du processus de confirmation de l'existence d'un répertoire dans le système de fichiers avant de le manipuler. C'est crucial pour éviter des erreurs en essayant d'accéder à un répertoire qui n'existe pas.


## Comment faire :
Dans Kotlin, on utilise la méthode `exists()` de la classe `File` pour vérifier l'existence d'un répertoire. Si le répertoire existe, elle renvoie `true`, sinon `false`.

Je vais vous montrer comment faire dans le code ci-dessous :

```Kotlin
import java.io.File

fun main() {
    val dir = File("/chemin/vers/le/répertoire")
    
    if (dir.exists()) {
        println("Le répertoire existe.")
    } else {
        println("Le répertoire n'existe pas.")
    }
}
```

Lors de l'exécution, si le répertoire existe, vous verrez le message "Le répertoire existe." sinon "Le répertoire n'existe pas."

## Approfondissement :

Historiquement, l'existence d'un répertoire a toujours été vérifiée avant son utilisation, pour éviter les erreurs. Kotlin s'inscrit dans cette tradition bien établie.

Il existe des alternatives à la méthode `exists()`. Par exemple, vous pouvez également utiliser la méthode `isDirectory()` qui vérifie non seulement si le répertoire existe, mais aussi s'il s'agit réellement d'un répertoire et non d'un fichier.

Lorsque vous utilisez `exists()`, notez que la vérification est effectuée lors de l'appel de la méthode et non lors de la création de l'objet `File`. Cela signifie que le statut d'existence peut changer entre la création de l'objet et l'appel de `exists()`. Gardez cela à l'esprit lors de la manipulation de fichiers et de répertoires.

## Voir également :
Pour plus d'informations et d'exemples en Kotlin, consultez les liens suivants:
- Documentation officielle de Kotlin : https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/
- Pour plus d'informations sur la classe `File` : https://www.geeksforgeeks.org/kotlin-file-class/
- Un guide pour manipuler des fichiers et des dossiers en Kotlin : https://www.baeldung.com/kotlin-file-io