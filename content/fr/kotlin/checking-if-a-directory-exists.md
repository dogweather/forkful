---
title:    "Kotlin: Vérifier si un répertoire existe"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Pourquoi il est important de vérifier l'existence d'un répertoire ?

Lorsqu'on programme en Kotlin, il est essentiel de vérifier si un répertoire existe avant d'effectuer une action qui pourrait potentiellement échouer. Cela permet d'éviter des erreurs et des plantages qui peuvent être coûteux en temps et en ressources.

## Comment vérifier si un répertoire existe en Kotlin

Pour vérifier si un répertoire existe en Kotlin, il suffit d'utiliser la fonction `exists()` de l'objet `File`. Voici un exemple de code:

```Kotlin
val directory = File("/chemin/vers/le/répertoire")

if (directory.exists()) {
    println("Le répertoire existe.")
} else {
    println("Le répertoire n'existe pas.")
}
```

Dans cet exemple, nous avons créé un objet `File` représentant un répertoire dont le chemin est spécifié en tant que paramètre. Ensuite, nous utilisons la fonction `exists()` pour vérifier si le répertoire existe. Selon le résultat, nous affichons un message approprié.

Lorsque nous exécutons ce code, nous obtenons l'un des deux messages suivants:

```
Le répertoire existe.
```

ou

```
Le répertoire n'existe pas.
```

Il est également possible d'utiliser la fonction `exists()` sur un objet `File` représentant un fichier pour vérifier l'existence de ce fichier.

## Plongée en profondeur

La fonction `exists()` vérifie si un fichier ou un répertoire existe actuellement sur le système de fichiers. Cependant, elle ne garantit pas que le fichier ou le répertoire sera toujours accessible par la suite. En effet, le fichier ou le répertoire peut être supprimé ou son accès peut être restreint par des permissions.

Pour une vérification plus approfondie, il est recommandé d'utiliser la fonction `canRead()` pour vérifier si un fichier ou un répertoire peut être lu et `canWrite()` pour vérifier s'il peut être modifié. Il est également important de prendre en compte les différentes exceptions qui peuvent être levées lors de la vérification de l'existence d'un fichier ou d'un répertoire.

## Voir aussi

- Documentation officielle de Kotlin sur l'utilisation de la classe `File`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/
- Exemples de codes pour vérifier l'existence d'un fichier ou d'un répertoire en Kotlin: https://www.tutorialkart.com/kotlin/working-with-files-in-kotlin/
- Ressources utiles pour apprendre Kotlin: https://blog.jetbrains.com/kotlin/2019/07/kotlin-microscope-useful-idioms/