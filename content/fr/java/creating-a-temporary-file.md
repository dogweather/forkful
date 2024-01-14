---
title:                "Java: Création d'un fichier temporaire"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est un aspect important de la programmation en Java. Lorsque vous travaillez avec des données temporaires ou des fichiers volumineux, il est souvent plus efficace de les stocker dans un fichier temporaire plutôt que de les conserver en mémoire. Créer des fichiers temporaires peut également être utile pour les tests et le débogage de vos applications.

## Comment

Pour créer un fichier temporaire en Java, vous pouvez utiliser la classe `java.io.File` et ses méthodes `createTempFile()`. Voici un exemple de code :

```java
// Créer un fichier temporaire avec un préfixe, un suffixe et un emplacement spécifiés
File tempFile = File.createTempFile("exemple", ".txt", new File("/chemin/vers/dossier/temp"));

// Écrire dans le fichier temporaire
try (PrintWriter writer = new PrintWriter(tempFile)) {
    writer.println("Ceci est un exemple de contenu dans un fichier temporaire");
} catch (IOException e) {
    e.printStackTrace();
}

// Lire le contenu du fichier temporaire
try (BufferedReader reader = new BufferedReader(new FileReader(tempFile))) {
    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }
} catch (IOException e) {
    e.printStackTrace();
}
```

La méthode `createTempFile()` prend en paramètres un préfixe, un suffixe et un emplacement où le fichier temporaire sera créé. Ensuite, vous pouvez écrire et lire le contenu du fichier temporaire comme vous le feriez avec n'importe quel autre fichier.

## Deep Dive

Il est important de noter que les fichiers temporaires sont supprimés lorsque l'application se termine, sauf si vous spécifiez le paramètre `deleteOnExit()` lors de la création du fichier. Vous pouvez également supprimer manuellement un fichier temporaire en utilisant la méthode `delete()`. De plus, il est possible d'utiliser des fichiers temporaires dans une application multi-thread sans risquer de conflits en utilisant des noms de fichier générés aléatoirement pour chaque thread.

## Voir aussi

Pour en savoir plus sur la gestion des fichiers temporaires en Java, vous pouvez consulter les liens suivants :

- [Java File class documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [How to create a temporary file in Java](https://www.baeldung.com/java-temporary-file)
- [Working with temporary files in Java](https://www.geeksforgeeks.org/working-with-temporary-files-in-java/)