---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:20.716569-07:00
description: "Travailler avec des fichiers CSV implique la lecture et l'\xE9criture\
  \ de donn\xE9es dans des fichiers aux valeurs s\xE9par\xE9es par des virgules (CSV),\
  \ un format\u2026"
lastmod: '2024-03-11T00:14:31.619346-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV implique la lecture et l'\xE9criture de\
  \ donn\xE9es dans des fichiers aux valeurs s\xE9par\xE9es par des virgules (CSV),\
  \ un format\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Travailler avec des fichiers CSV implique la lecture et l'écriture de données dans des fichiers aux valeurs séparées par des virgules (CSV), un format populaire pour l'échange de données car il est simple et largement pris en charge. Les programmeurs manipulent les fichiers CSV pour des tâches telles que l'import/export de données, l'analyse de données et le partage d'informations entre différents systèmes.

## Comment faire :

### Lire un fichier CSV en utilisant la bibliothèque standard de Java

Java ne dispose pas d'un support intégré pour les CSV dans sa bibliothèque standard, mais vous pouvez facilement lire un fichier CSV en utilisant les classes de `java.io`.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Spécifiez le chemin vers le fichier CSV
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // En supposant qu'une virgule est le délimiteur
                // Traiter les données
                for (String value : values) {
                    System.out.print(value + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Écrire dans un fichier CSV en utilisant la bibliothèque standard de Java

Pour écrire des données dans un fichier CSV, vous pouvez utiliser des classes de `java.io` telles que `FileWriter` et `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Spécifiez le chemin vers le fichier CSV de sortie

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // En supposant qu'une virgule est le délimiteur
            }
            sb.deleteCharAt(sb.length() - 1); // Supprimer la dernière virgule
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Utiliser une bibliothèque tierce : Apache Commons CSV

Apache Commons CSV est une bibliothèque populaire pour manipuler les fichiers CSV en Java. Elle simplifie de manière significative la lecture et l'écriture des fichiers CSV.

Ajoutez la dépendance à votre projet :

Pour Maven :

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Vérifiez la dernière version -->
</dependency>
```

#### Lire un fichier CSV :

```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.Reader;
import java.io.FileReader;
import java.io.IOException;

public class ApacheReadCSVExample {
    public static void main(String[] args) {
        String csvFile = "data.csv";
        try (Reader reader = new FileReader(csvFile);
             CSVParser csvParser = new CSVParser(reader, CSVFormat.DEFAULT)) {
            for (CSVRecord csvRecord : csvParser) {
                // Accéder aux valeurs par les indices de colonnes
                String columnOne = csvRecord.get(0);
                String columnTwo = csvRecord.get(1);
                System.out.println(columnOne + " " + columnTwo);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

#### Écrire dans un fichier CSV :

```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"First Name", "Last Name", "Age", "City"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // Le casting en Object[] est nécessaire ici
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV gère automatiquement les complexités telles que les guillemets et les virgules dans les champs, ce qui en fait un choix robuste pour la manipulation de CSV en Java.
