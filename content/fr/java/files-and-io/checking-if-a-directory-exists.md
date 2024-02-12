---
title:                "Vérifier si un répertoire existe"
aliases:
- /fr/java/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:12.395238-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Vérifier si un répertoire existe en Java est une tâche fondamentale qui implique de vérifier la présence d'un répertoire du système de fichiers avant de le lire, d'écrire dedans ou d'effectuer toute opération nécessitant son existence. Cela est crucial pour éviter les erreurs ou les exceptions dans les programmes qui interagissent avec le système de fichiers, assurant une exécution plus fluide et une meilleure expérience utilisateur.

## Comment faire :
En Java, il existe plusieurs manières de vérifier si un répertoire existe, principalement en utilisant les classes `java.nio.file.Files` et `java.io.File`.

**En utilisant `java.nio.file.Files`** :

Ceci est l'approche recommandée dans les versions récentes de Java.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // Spécifier ici le chemin du répertoire
        String directoryPath = "chemin/vers/repertoire";

        // Vérification de l'existence du répertoire
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("Le répertoire existe.");
        } else {
            System.out.println("Le répertoire n'existe pas.");
        }
    }
}
```
**Sortie d'exemple** :
```
Le répertoire existe.
```
Ou 
```
Le répertoire n'existe pas.
```

**En utilisant `java.io.File`** :

Bien que `java.nio.file.Files` soit recommandé, l'ancienne classe `java.io.File` peut également être utilisée.

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // Spécifier ici le chemin du répertoire
        String directoryPath = "chemin/vers/repertoire";

        // Création d'un objet File
        File directory = new File(directoryPath);

        // Vérification de l'existence du répertoire
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("Le répertoire existe.");
        } else {
            System.out.println("Le répertoire n'existe pas.");
        }
    }
}
```
**Sortie d'exemple** :
```
Le répertoire existe.
```
Ou
```
Le répertoire n'existe pas.
```

**Utilisation de bibliothèques tierces** :

Bien que la bibliothèque standard Java soit généralement suffisante pour cette tâche, des bibliothèques tierces comme Apache Commons IO offrent des utilitaires de manipulation de fichiers supplémentaires qui pourraient être utiles dans des applications plus complexes.

**Apache Commons IO** :

D'abord, ajoutez la dépendance Apache Commons IO à votre projet. Ensuite, vous pouvez utiliser ses fonctionnalités pour vérifier l'existence d'un répertoire.

```java
// En supposant qu'Apache Commons IO est ajouté au projet

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // Spécifier ici le chemin du répertoire
        String directoryPath = "chemin/vers/repertoire";

        // Utilisation de FileUtils pour vérifier
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("Le répertoire existe.");
        } else {
            System.out.println("Le répertoire n'existe pas.");
        }
    }
}
```

**Note** : `FileUtils.directoryContains` vérifie si un répertoire contient un fichier spécifique, mais en passant `null` comme second argument, vous pouvez l'utiliser pour vérifier l'existence du répertoire. Soyez prudent, car cela pourrait ne pas être l'utilisation la plus simple ou la plus intentionnelle de la méthode.

**Sortie d'exemple** :
```
Le répertoire existe.
```
Ou
```
Le répertoire n'existe pas.
```
