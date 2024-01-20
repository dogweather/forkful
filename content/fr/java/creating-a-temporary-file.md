---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

---

## Qu'est-ce que c'est & Pourquoi?

La création d'un fichier temporaire consiste simplement à créer un fichier qui ne doit exister que pendant une courte période. Les programmeurs le font surtout pour un stockage provisoire de données ou pour un transfert entre deux applications ou systèmes.

## Comment faire:

Voici comment vous pouvez créer un fichier temporaire en Java:

```Java
import java.io.File;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        try {
            File tempFile = File.createTempFile("tempFileExample", ".txt");

            System.out.println("Temp file : " + tempFile.getAbsolutePath());

            boolean exists = tempFile.exists();

            System.out.println("Does file exist? : " + exists);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Et la sortie sera:

```Java
Temp file : C:\Users\Username\AppData\Local\Temp\tempFileExample1234567890.txt
Does file exist? : true
```

## Plongée profonde

Historiquement, la création de fichiers temporaires en Java a commencé avec Java 2. Avant, les programmeurs devaient généralement manipuler le système d'exploitation pour obtenir le même résultat.

En termes d'alternatives, vous pourriez utiliser les bibliothèques externe et très populaires comme Apache Commons IO.

En tout état de cause, il est important de savoir que `File.createTempFile` crée le fichier dans le répertoire temporaire du système. Vous pouvez modifier cela en provided votre propre chemin lors de la création du `File` si vous le souhaitez.

## Pour en savoir plus

Pour en savoir plus sur la création de fichiers temporaires en Java:

- [Oracle Java Documentation: File (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [StackOverflow: Java Create Temp File](https://stackoverflow.com/questions/16624982/java-create-temp-file)