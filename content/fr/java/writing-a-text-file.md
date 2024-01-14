---
title:                "Java: Écriture d'un fichier texte."
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte est une tâche courante en programmation Java. Cela permet de stocker des données de manière structurée et organisée, et de les récupérer rapidement lorsque nécessaire. Dans cet article, nous allons expliquer pourquoi il est important et utile d'écrire des fichiers texte en Java.

## Comment faire

Voici un exemple de code en Java pour écrire un fichier texte :

```Java
import java.io.FileWriter;
import java.io.IOException;

public class FileWritingExample {

    public static void main(String[] args) {

        FileWriter writer = null;

        try {
            writer = new FileWriter("monFichier.txt"); // chemin de destination du fichier

            writer.write("Bonjour à tous !"); // contenu du fichier
            writer.write("\nCeci est un exemple de texte écrit en Java.");

            System.out.println("Fichier créé avec succès !");
            
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                writer.close(); // fermeture du fichier
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
```

Le code ci-dessus utilise la classe `FileWriter` pour écrire dans un fichier texte. Nous devons d'abord instancier un objet `FileWriter` en lui passant en paramètre le chemin du fichier que nous souhaitons créer ou modifier. Ensuite, nous pouvons utiliser la méthode `write()` pour insérer du contenu dans le fichier. Enfin, nous fermons le fichier en utilisant la méthode `close()`. 

Voici ce que nous obtiendrons à l'exécution de ce code :

```
Bonjour à tous !
Ceci est un exemple de texte écrit en Java.
```

Il est également possible d'écrire dans un fichier texte en utilisant la méthode `append()` de la classe `StringBuilder` et en utilisant un objet `PrintWriter` pour écrire dans le fichier. Voici un exemple de code :

```Java
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class FileWritingExample2 {

    public static void main(String[] args) {

        StringBuilder sb = new StringBuilder();

        sb.append("Bonjour à tous !");
        sb.append("\nCeci est un exemple de texte écrit en Java.");

        File file = new File("monFichier.txt"); // chemin de destination du fichier

        try (PrintWriter writer = new PrintWriter(new FileWriter(file))) {
            writer.print(sb.toString()); // écriture du contenu dans le fichier
            System.out.println("Fichier créé avec succès !");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Ce code produit le même résultat que le précédent.

## Plongée en profondeur

Pour écrire un fichier texte, il est également important de savoir comment gérer les exceptions. La méthode `write()` peut générer une `IOException` si quelque chose ne se passe pas comme prévu lors de l'écriture dans le fichier. C'est pourquoi nous devons entourer notre code avec un bloc `try-catch` pour gérer toute erreur éventuelle.

Il est également bon de savoir qu'il existe d'autres classes pour écrire dans un fichier, comme `OutputStreamWriter` et `BufferedWriter`. Il est important de se familiariser avec ces différentes classes et de choisir celle qui correspond le mieux à nos besoins.

## Voir aussi

Pour en savoir plus sur l'écriture de fichiers en utilisant Java, voici quelques liens utiles :

- Tutoriel Java sur l'écriture de fichiers : https://openclassrooms.com/fr/courses/1904416-programmez-en-oriente-objet-en-java/1908771-manipulez-des-fichiers-et-des-flux-de-donnees
- Documentation officielle de Java sur la classe `FileWriter` : https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html
- Tutoriel vidéo sur l'utilisation de la classe `PrintWriter` : https://www.youtube.com/watch?v=bI6RfN8w1Hg