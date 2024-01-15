---
title:                "Travailler avec des fichiers CSV"
html_title:           "Java: Travailler avec des fichiers CSV"
simple_title:         "Travailler avec des fichiers CSV"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi 
CSV, ou Comma-Separated Values, est un format couramment utilisé pour stocker et échanger des données tabulaires. Il est simple et convivial, ce qui en fait un choix populaire lors de la manipulation de données dans le développement de logiciels.

## Comment faire 
La manipulation de données CSV en Java est assez simple et peut être réalisée en utilisant des packages tels que "java.io" et "java.util". Voici un exemple de code pour lire un fichier CSV et afficher son contenu:

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class CSVReaderExample {
    public static void main(String[] args) {
        String csvfile = "exemples/etudiants.csv";
        String line = "";

        try (BufferedReader br = new BufferedReader(new FileReader(csvfile))) {
            while ((line = br.readLine()) != null) {
                String[] data = line.split(",");
                System.out.println("Etudiant: " + data[0] + ", Age: " + data[1] + ", Classe: " + data[2]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

La sortie de ce code sera:

```
Etudiant: Jean, Age: 20, Classe: INFO1
Etudiant: Marie, Age: 21, Classe: INFO1
Etudiant: Pierre, Age: 19, Classe: INFO2
```

## Plongée en profondeur 
Il existe des bibliothèques tierces telles que Apache Commons CSV et OpenCSV qui offrent des fonctionnalités avancées pour travailler avec des fichiers CSV en Java. Ces bibliothèques permettent de gérer plus facilement les erreurs de format et les types de données, ainsi que de manipuler les données en utilisant des objets plutôt que des tableaux de chaînes.

Il est également important de noter que les fichiers CSV peuvent avoir des délimiteurs autres que les virgules, et que ces bibliothèques peuvent être configurées pour reconnaître différents délimiteurs.

## Voir aussi 
- [Tutoriel sur la manipulation de fichiers CSV en Java](https://www.baeldung.com/java-csv-file-array)
- [Documentation sur Apache Commons CSV](http://commons.apache.org/proper/commons-csv/)
- [Documentation sur OpenCSV](http://opencsv.sourceforge.net/)