---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
CSV signifie "Comma-separated values", c'est un format de fichier utilisé pour stocker des données tabulaires. Les développeurs l'utilisent pour sa simplicité et sa facilité d'intégration avec des outils de traitement de données.

## Comment faire :
```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class CSVReader {
    public static void main(String[] args) {
        String path = "data.csv";
        String line;

        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                // Affiche les valeurs lignes par ligne
                System.out.println("Colonne 1: " + values[0] + ", Colonne 2: " + values[1]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Sortie attendue:
```
Colonne 1: valeur1, Colonne 2: valeur2
Colonne 1: valeur3, Colonne 2: valeur4
...
```

## Plongeon :
Historiquement, CSV est l'un des formats les plus anciens et les plus universellement reconnus pour l'échange de données. Des alternatives existent, comme JSON ou XML, mais CSV reste populaire, surtout pour les grandes quantités de données simples. En Java, pour aller plus loin que la lecture basique, des bibliothèques comme Apache Commons CSV ou OpenCSV fournissent des fonctionnalités avancées et une meilleure gestion des erreurs.

## Voir Aussi :
- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- OpenCSV: http://opencsv.sourceforge.net/
- Documentation Oracle sur les I/O en Java : https://docs.oracle.com/en/java/javase/19/docs/api/java.base/java/io/package-summary.html
