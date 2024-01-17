---
title:                "Écrire un fichier texte"
html_title:           "Java: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Ecrire un fichier texte en Java est une façon de stocker des informations textuelles de manière organisée et accessible. Les programmeurs l'utilisent souvent pour sauvegarder des données ou pour créer des rapports à partir de leur code.

## Comment Faire:
Pour écrire un fichier texte en Java, utilisez la classe `FileWriter` pour créer un nouveau fichier et la méthode `write()` pour y ajouter du contenu. Voici un exemple de code qui écrit une ligne de texte dans un fichier texte:

```Java
import java.io.*;

public class TextWriter {
    public static void main(String[] args) {
        try {
            FileWriter fWriter = new FileWriter("monFichier.txt");
            fWriter.write("Bonjour, monde!");
            fWriter.close();
        } catch(IOException e) {
            System.out.println("Erreur lors de l'écriture du fichier.");
        }
    }
}
```

Lorsque vous exécutez ce code, il va créer un fichier appelé "monFichier.txt" et y écrire la ligne de texte. Vous pouvez également utiliser la méthode `append()` pour ajouter du contenu à un fichier existant plutôt que de le remplacer entièrement.

## Plongée Profonde:
Ecrire des fichiers texte en Java est disponible depuis les premières versions du langage, et reste une méthode simple et efficace pour stocker des données. Cependant, avec l'avènement de bases de données et de formats de fichiers plus performants, comme le JSON ou le XML, il peut être plus approprié d'utiliser ces alternatives dans certains cas.

De plus, l'implémentation de la classe `FileWriter` peut varier selon le système d'exploitation sur lequel le code est exécuté. Il est donc important de prendre en compte ces différences lors de la création de fichiers texte dans un code destiné à être utilisé sur plusieurs plateformes.

## Voir Aussi:
Vous pouvez en apprendre davantage sur l'écriture de fichiers texte en Java en consultant la documentation officielle: https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html

Pour une comparaison entre l'utilisation de fichiers texte et de bases de données, consultez cet article: https://www.baeldung.com/java-file-vs-database

Et si vous souhaitez explorer d'autres façons d'écrire du contenu dans un fichier en Java, lisez ceci: http://www.journaldev.com/878/java-write-to-file