---
title:                "Travailler avec des fichiers csv"
html_title:           "Java: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Travailler avec des fichiers CSV est une tâche courante pour les programmeurs Java. Un fichier CSV (Comma Separated Values) est un type de fichier utilisé pour stocker des données sous forme de colonnes et de lignes, séparées par des virgules.

Les programmeurs utilisent des fichiers CSV pour stocker et manipuler facilement de grandes quantités de données. Cela peut inclure des données provenant de feuilles de calcul, de bases de données ou d'autres sources. Les fichiers CSV sont également utilisés pour échanger des données entre différentes applications.

## Comment faire:

La manipulation de fichiers CSV en Java peut être accomplie en utilisant des bibliothèques telles que OpenCSV ou Apache Commons CSV. Voici un exemple de code utilisant Apache Commons CSV pour lire et écrire des données à partir d'un fichier CSV :

```
import java.io.IOException;
import org.apache.commons.csv.*;

public class CsvExample {

    public static void main(String[] args) throws IOException {

        // lecture des données à partir d'un fichier CSV
        CSVParser parser = CSVParser.parse(new File("donnees.csv"), Charset.defaultCharset(), CSVFormat.RFC4180);
        
        for (CSVRecord record : parser) {
            // récupère chaque colonne de la ligne actuelle
            String col1 = record.get(0);
            String col2 = record.get(1);
            String col3 = record.get(2);

            // fait quelque chose avec les données récupérées
        }

        // écriture des données dans un fichier CSV
        FileWriter writer = new FileWriter("donnees.csv");
        CSVPrinter printer = new CSVPrinter(writer, CSVFormat.RFC4180);
        printer.printRecord("Colonne 1", "Colonne 2", "Colonne 3");
        printer.printRecord("Donnée 1", "Donnée 2", "Donnée 3");
        printer.printRecord("Donnée 4", "Donnée 5", "Donnée 6");

        printer.close();
    }
}
```

Le code ci-dessus utilise un parseur CSV pour lire les données à partir d'un fichier CSV et un imprimante CSV pour écrire des données dans un fichier CSV.

## Deep Dive:

Les fichiers CSV ont été populaires depuis longtemps en raison de leur simplicité et de leur compatibilité avec un large éventail d'applications. Cependant, ils peuvent poser des problèmes avec les données contenant des virgules ou des guillemets, qui doivent être échappés pour être correctement lus.

Dans les versions plus anciennes de Java, les développeurs devaient écrire leur propre code pour manipuler les fichiers CSV. Mais avec l'avènement de bibliothèques dédiées telles que OpenCSV et Apache Commons CSV, la manipulation de fichiers CSV en Java est devenue plus facile et plus fiable.

## Voir aussi:

- [OpenCSV documentation](http://opencsv.sourceforge.net/)
- [Apache Commons CSV documentation](https://commons.apache.org/proper/commons-csv/)