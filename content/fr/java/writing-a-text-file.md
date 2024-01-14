---
title:                "Java: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création d'un fichier texte est une tâche courante dans la programmation Java. Que ce soit pour stocker des données, générer des rapports ou créer des fichiers de configuration, la création de fichiers texte est une compétence importante pour tout programmeur Java. Dans cet article, nous allons explorer comment écrire un fichier texte en Java et pourquoi c'est une compétence utile à avoir.

## Comment faire

Pour écrire un fichier texte en Java, nous allons utiliser la classe `FileWriter` et la classe `BufferedWriter` pour améliorer les performances. Voici un exemple de code :

```java
import java.io.*;

public class EcrireFichier {

    public static void main(String[] args) {
    
        // Créer un objet FileWriter
        FileWriter writer = null;
        
        try {
            // Spécifier le chemin et le nom du fichier
            File fichier = new File("monFichier.txt");
            // Initialiser l'objet FileWriter avec le fichier
            writer = new FileWriter(fichier);
            // Créer un objet BufferedWriter pour améliorer les performances
            BufferedWriter br = new BufferedWriter(writer);
            // Écrire du texte dans le fichier
            br.write("Bonjour les programmeurs Java !");
            // Fermer le BufferedWriter
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    
    }

}
```

Le code ci-dessus va créer un fichier texte nommé "monFichier.txt" dans le même répertoire que votre programme Java. Vous pouvez vérifier le contenu du fichier en utilisant un éditeur de texte.

## Plongée en profondeur

Il est important de noter que lors de l'écriture de fichiers texte en Java, il est nécessaire de gérer les exceptions. Cela peut être fait à l'aide de la clause `try/catch` comme dans l'exemple ci-dessus. Il est également important de fermer le `BufferedWriter` après avoir écrit dans le fichier, sinon les données peuvent ne pas être écrites correctement.

De plus, vous pouvez également spécifier un chemin absolu au lieu d'un chemin relatif pour créer votre fichier texte. Et si vous voulez ajouter du texte à un fichier existant, vous pouvez utiliser la méthode `append()` sur l'objet `FileWriter` au lieu de créer un nouveau fichier.

## Voir aussi

Consultez ces liens pour en savoir plus sur l'écriture de fichiers texte en Java :

- [Guide Oracle Java - Les entrées/sorties (E/S)](https://docs.oracle.com/javase/tutorial/essential/io/)
- [Tutoriel journal des activités Java - Lecture et écriture de fichiers](https://www.baeldung.com/java-write-to-file)
- [Programmez facilement en Java - Écriture de fichiers](https://openclassrooms.com/fr/courses/26832-apprenez-a-programmer-en-java/23428-le-modele-io-en-java#/id/r-23644)

Écrivez de jolis fichiers texte en Java et améliorez votre codage dès aujourd'hui !