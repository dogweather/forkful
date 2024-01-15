---
title:                "Vérifier si un répertoire existe"
html_title:           "Kotlin: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous travaillez avec des fichiers et des dossiers dans vos applications Kotlin, il est important de vérifier l'existence d'un dossier avant de le manipuler. Cela permet d'éviter les erreurs et les plantages de l'application.

## Comment faire 

Pour vérifier si un dossier existe en utilisant Kotlin, il faut d'abord importer la classe "File" à partir de la bibliothèque standard de Kotlin. Ensuite, vous pouvez utiliser la méthode "exists()" pour vérifier l'existence du dossier. Voici un exemple de code avec une sortie d'exemple : 

```Kotlin 
import java.io.File 

fun main() { 
    val dossier = File("chemin/vers/dossier") 
    
    if(dossier.exists()){ 
        println("Le dossier existe !") 
    } else { 
        println("Le dossier n'existe pas.") 
    } 
} 

// Output : Le dossier existe ! 
``` 

## Deep Dive 

Lorsque vous utilisez la méthode "exists()" pour vérifier l'existence d'un dossier, il est important de noter que cette méthode ne vérifie que l'existence du dossier spécifié et non son contenu. Cela signifie que si un fichier portant le même nom que le dossier existe, la méthode renverra quand même "true".

De plus, la méthode "exists()" peut également être utilisée pour vérifier l'existence d'un fichier. Dans ce cas, elle vérifiera uniquement l'existence du fichier spécifié et non s'il s'agit d'un dossier.

## Voir aussi 

- [Documentation officielle de Kotlin sur la classe File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Exemple de code pour vérifier si un fichier existe en utilisant Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html#&lt;init&gt;(kotlin.String):kotlin.io.File#44;exists)
- [Exemple de code pour vérifier l'existence d'un dossier et de son contenu en utilisant Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html#44;exists)