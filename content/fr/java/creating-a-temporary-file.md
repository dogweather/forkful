---
title:    "Java: Créer un fichier temporaire"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Pourquoi

Créer un fichier temporaire peut être un outil utile lors de la programmation en Java. Un fichier temporaire est un fichier qui est créé pour contenir des données temporaires et qui est ensuite supprimé une fois qu'il n'est plus nécessaire. Cette pratique peut être particulièrement utile lors de la manipulation de grandes quantités de données ou lors de la création de fichiers de sauvegarde temporaires.

## Comment faire

Pour créer un fichier temporaire en Java, vous pouvez utiliser la classe `File` et la méthode `createTempFile()`, qui prend en paramètre un préfixe de nom de fichier et un suffixe pour spécifier le type de fichier. Voici un exemple de code :

```java
// Importer la classe File
import java.io.File;

// Créer un fichier temporaire avec un préfixe "test" et un suffixe ".txt"
File tempFile = File.createTempFile("test", ".txt");

// Afficher le chemin absolu du fichier temporaire créé
System.out.println(tempFile.getAbsolutePath());
```

La sortie de ce code sera quelque chose comme : `/var/folders/my/test1234567890.txt`. Vous pouvez également spécifier un répertoire de stockage pour le fichier temporaire en utilisant la méthode surchargée `createTempFile(prefix, suffix, directory)`.

Il est important de noter que le fichier temporaire n'est pas créé dans le répertoire spécifié, mais dans le répertoire des fichiers temporaires du système d'exploitation. Vous pouvez y accéder en utilisant la méthode `getAbsolutePath()` comme indiqué dans l'exemple ci-dessus.

## Plongée profonde

Il existe plusieurs options supplémentaires pour personnaliser la création d'un fichier temporaire en Java. Par exemple, vous pouvez spécifier un `Charset` pour le contenu du fichier en utilisant la méthode `createTempFile(prefix, suffix, directory, charset)`. Vous pouvez également fournir un `FileAttribute` pour modifier les attributs du fichier créé.

De plus, vous pouvez utiliser la méthode `deleteOnExit()` pour supprimer automatiquement le fichier temporaire lors de la fermeture de l'application. Vous pouvez également supprimer explicitement le fichier en appelant la méthode `delete()` sur l'instance de fichier.

##Voir aussi

Pour plus d'informations sur la manipulation de fichiers en Java, vous pouvez consulter les ressources suivantes :

- Tutoriel sur les fichiers et les flux en Java : [https://www.baeldung.com/java-io](https://www.baeldung.com/java-io)
- Documentation officielle de Java sur la classe `File` : [https://docs.oracle.com/javase/8/docs/api/java/io/File.html](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- Tutoriel sur la manipulation de fichiers en Java : [https://www.journaldev.com/19187/java-create-file](https://www.journaldev.com/19187/java-create-file)

Maintenant que vous savez comment créer un fichier temporaire en Java, vous pouvez l'utiliser dans vos projets pour une manipulation efficace des données temporaires. N'hésitez pas à explorer davantage les options et les possibilités offertes par la classe `File` pour personnaliser votre utilisation de fichiers temporaires.