---
title:    "Java: Lecture d'un fichier texte"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Java, il est fort probable que vous ayez déjà dû lire des fichiers texte dans vos projets. Les fichiers texte sont un moyen courant de stocker des données et de les lire dans une application. Dans cet article, nous allons explorer comment lire efficacement un fichier texte en utilisant Java.

## Comment

Pour lire un fichier texte en Java, nous allons utiliser la classe InputStreamReader et BufferedReader. Voici un exemple de code qui lit un fichier texte et affiche son contenu sur la console :

```Java
import java.io.*;

public class LectureFichier {

    public static void main(String[] args) {
        File fichier = new File("test.txt");
        try {
            FileInputStream fis = new FileInputStream(fichier);
            InputStreamReader isr = new InputStreamReader(fis);
            BufferedReader br = new BufferedReader(isr);

            String ligne;
            while ((ligne = br.readLine()) != null) {
                System.out.println(ligne);
            }

            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

En utilisant la méthode `readLine()` de la classe BufferedReader, nous pouvons lire chaque ligne du fichier et l'afficher sur la console. N'oubliez pas de fermer le BufferedReader après avoir terminé de lire le fichier.

Lors de l'exécution de ce code, vous devriez voir le contenu du fichier `test.txt` s'afficher sur la console.

## Plongée en profondeur

La classe BufferedReader offre également d'autres méthodes utiles pour lire des fichiers texte, telles que `read()`, qui lit un seul caractère, ou encore `skip()`, qui permet de sauter un nombre spécifié de caractères. Vous pouvez également spécifier un paramètre de taille de buffer pour la classe BufferedReader afin d'optimiser les performances de lecture.

De plus, en utilisant la classe FileWriter et BufferedWriter, vous pouvez également écrire dans un fichier texte de manière similaire. Assurez-vous de fermer le BufferedWriter après avoir terminé d'écrire dans le fichier.

## Voir aussi

- [Documentation sur la classe BufferedReader en français] (https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Tutoriel en français sur la lecture et l'écriture de fichiers en Java] (https://openclassrooms.com/fr/courses/6173501-apprenez-les-bases-du-developpement-dapplications-avec-java/6228911-gerez-des-donnees-persistantes-dans-vos-applications-avec-java)

Merci d'avoir lu cet article sur la lecture de fichiers texte en Java. Nous espérons que cela vous a été utile dans vos projets de développement. N'hésitez pas à consulter les liens ci-dessus pour en savoir plus sur la manipulation de fichiers en Java. À bientôt !