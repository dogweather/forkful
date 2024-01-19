---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?

La création d'un fichier temporaire est une opération qui consiste à générer un fichier de stockage provisoire lors de l'exécution d'un programme. Les développeurs y recourent pour stocker temporairement des données ou pour partager des informations entre différents processus.

## Comment faire :

À l'aide de Kotlin, la création d'un fichier temporaire est simple et effective. Voici un exemple de code montrant comment réaliser cela :

```Kotlin
import java.io.File

fun main() {
   val tempFile = File.createTempFile("tempFile", ".txt")

   println("Le fichier temporaire est créé à : ${tempFile.absolutePath}")
}
```

Après avoir exécuté ce script, vous obtiendriez quelque chose comme ceci:

```
Le fichier temporaire est créé à : C:\Users\votrePC\AppData\Local\Temp\tempFile5641517977453134434.txt
```

## Détails Approfondis :

- **Contexte historique** : Auparavant, le concept de fichiers temporaires existait déjà dans divers langages de programmation. Kotlin, étant un langage jeune, s'appuie sur les principes existants de Java.

- **Alternatives** : On peut également gérer les fichiers temporaires avec d'autres librairies, comme Apache Commons IO. Mais le standard Java/Kotlin est suffisamment puissant pour la majorité des usages.

- **Détails d'implémentation** : Dans notre exemple, la méthode `createTempFile` prend deux arguments - le préfixe et le suffixe du nom du fichier temporaire. Le fichier créé est effacé du disque lors de la suppression de l'objet File ou lors de l'arrêt de la JVM.

## Voir Aussi :

>- Pour plus d'informations sur la création de fichiers temporaires, consultez la [documentation officielle de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/create-temp-file.html).
>- Consultez la [documentation Java](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#deleteOnExit()) pour comprendre comment Java gère les fichiers temporaires.
>- Pour en savoir plus sur la bibliothèque Apache Commons IO, rendez-vous sur leur [site officiel](https://commons.apache.org/proper/commons-io/).