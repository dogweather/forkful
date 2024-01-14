---
title:    "Kotlin: Création d'un fichier temporaire"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi créer un fichier temporaire en Kotlin ?

Les fichiers temporaires sont utilisés dans la programmation pour stocker des données temporaires qui ne sont pas nécessaires à long terme. Que ce soit pour stocker des données intermédiaires, des fichiers de cache, ou pour une utilisation dans des tests, créer un fichier temporaire peut être utile lors de la création d'applications en Kotlin.

# Comment créer un fichier temporaire en Kotlin ?

La création d'un fichier temporaire en Kotlin est assez simple. Vous pouvez utiliser la fonction `createTempFile()` et lui passer en paramètre le préfixe de votre fichier, le suffixe et le répertoire de stockage. Voici un exemple de code :

```Kotlin
import java.io.File

fun main() {
    // Création d'un fichier temporaire avec un préfixe, un suffixe et stocké dans le répertoire courant
    val tempFile = File.createTempFile("mon_fichier", ".txt") 
    
    // Vous pouvez également spécifier le répertoire de stockage :
    // val tempFile = File.createTempFile("mon_fichier", ".txt", File("/chemin/vers/repertoire/"))
    
    // Affichage du chemin absolu du fichier temporaire 
    println(tempFile.absolutePath)
    
    // Vérification si le fichier existe
    if (tempFile.exists()) {
        // Suppression du fichier temporaire
        tempFile.delete()
    }
}
```

Voici ce que la sortie de ce code ressemblerait :

```
/C:/Users/utilisateur/AppData/Local/Temp/mon_fichier15166824181197465462.txt
```

# Plongée en profondeur : création d'un fichier temporaire en Kotlin

La méthode `createTempFile()` utilise en interne la classe `File` et crée le fichier avec le préfixe, le suffixe et le répertoire que vous lui avez passés en paramètres. Si vous n'avez pas spécifié de répertoire de stockage, le fichier temporaire sera créé dans le répertoire par défaut pour les fichiers temporaires de votre système d'exploitation. De plus, vous pouvez également spécifier un préfixe et un suffixe optionnels pour le nom de votre fichier temporaire. Si vous ne spécifiez pas ces paramètres, un nom aléatoire sera généré pour votre fichier.

# Voir aussi

- [Documentation officielle de Kotlin sur la création de fichiers temporaires](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Guide complet de la création de fichiers en Kotlin](https://www.baeldung.com/kotlin-create-file)
- [Utilisation de fichiers temporaires en TDD](https://spin.atomicobject.com/2016/07/26/temporary-files-tdd-kotlin/)