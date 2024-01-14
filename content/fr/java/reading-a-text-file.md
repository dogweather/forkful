---
title:    "Java: La lecture d'un fichier texte"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Pourquoi

Lire des fichiers texte est un aspect fondamental de la programmation en Java. Cela permet d'accéder à des données stockées dans des fichiers pour les utiliser dans nos programmes. Dans cet article, nous allons vous montrer comment lire des fichiers texte en Java et vous fournir des informations détaillées sur cette tâche.

## Comment faire

Pour lire un fichier texte en Java, nous avons besoin de quatre étapes principales : ouvrir le fichier, le lire ligne par ligne, traiter les données et enfin fermer le fichier. Voyons comment cela peut être fait en code :

```Java
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;

public class ReadTextFile {
    public static void main(String[] args) {
        // Ouvrir le fichier en utilisant un objet File
        File file = new File("monfichier.txt");

        // Définir un objet FileReader pour lire le fichier
        FileReader fr = new FileReader(file);

        // Définir un objet BufferedReader pour lire le contenu du fichier ligne par ligne
        BufferedReader br = new BufferedReader(fr);

        // Déclarer une variable pour stocker chaque ligne lue
        String line = "";

        // Lire le fichier ligne par ligne en utilisant une boucle
        while ((line = br.readLine()) != null) {
            // Traiter les données lues ici
            System.out.println(line);
        }

        // Fermer le fichier pour éviter les fuites de mémoire
        br.close();
        fr.close();
    } 
}
```

Voici un exemple de contenu qui peut se trouver dans le fichier `monfichier.txt` :

```
Jean Dupont
Marie Martin
Pierre Durand
```

La sortie de ce programme serait :

```
Jean Dupont
Marie Martin
Pierre Durand
```

## Deep Dive

La première étape consiste à ouvrir le fichier avec un objet `File`. Ceci peut être fait en fournissant le chemin du fichier en tant que paramètre au constructeur de `File`. Ensuite, nous déclarons un objet `FileReader` pour lire le contenu du fichier. Ce dernier est passé en paramètre au constructeur de `BufferedReader`. Le `BufferedReader` est utilisé pour lire le contenu du fichier ligne par ligne en utilisant la méthode `readLine()`. Il est important de noter que cette méthode renvoie `null` lorsqu'elle atteint la fin du fichier.

Dans notre exemple, nous avons simplement utilisé `System.out.println()` pour afficher le contenu de chaque ligne lue. Mais vous pouvez effectuer n'importe quelle opération sur ces données, telles que les stocker dans un tableau ou les utiliser pour créer de nouveaux objets.

## Voir aussi

- [Documentation officielle Java pour `BufferedReader`](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html)
- [Tutoriel sur la lecture et l'écriture de fichiers en Java](https://www.baeldung.com/java-write-to-file)
- [Exemples de lecture de fichiers en Java sur GitHub](https://github.com/search?q=java+read+text+file&type=Repositories)