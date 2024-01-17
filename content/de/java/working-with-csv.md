---
title:                "Arbeiten mit CSV"
html_title:           "Java: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-csv.md"
---

{{< edit_this_page >}}

# Was ist das und warum machen es Programmierer?

CSV steht für "Comma-Separated Values" und ist ein gebräuchliches Dateiformat zum Speichern von Daten in Form von Tabellen. Programmierer nutzen CSV-Dateien, um Daten schnell und effizient zu verarbeiten und zu importieren, insbesondere in Datenbanken oder Tabellenkalkulationen.

# Wie funktioniert es?

Im Folgenden wird gezeigt, wie man mit CSV-Dateien in Java arbeiten kann, anhand von zwei Beispielen. In beiden Beispielen lesen wir Daten aus einer CSV-Datei ein und speichern sie in einer Liste. Dann geben wir die Daten mithilfe der System.out.println() -Methode aus.

```
// Beispiel 1: CSV-Datei mit festgelegten Spalten einlesen

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class CSVReader {

    public static void main(String[] args) {

        // Name der CSV-Datei angeben
        String csvFile = "C:/Dateien/data.csv";

        // Trennzeichen für die CSV-Datei festlegen
        String delimiter = ",";

        // Neue Liste für die Daten erstellen
        List<String[]> data = new ArrayList<>();

        // Versuchen, die CSV-Datei zu lesen und die Daten in die Liste zu speichern
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {

            String line;
            while ((line = br.readLine()) != null) {

                // Zeile anhand des Trennzeichens in einzelne Spalten aufteilen
                String[] row = line.split(delimiter);

                // Daten in die Liste speichern
                data.add(row);

            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        // Daten ausgeben
        for (String[] row : data) {
            System.out.println("Name: " + row[0] + ", Alter: " + row[1] + ", Beruf: " + row[2]);
        }

    }

}
```

Beispiel-CSV-Datei "data.csv":

```
Max,25,Student
Lisa,30,Lehrer
Peter,40,Programmierer
```

Ausgabe:

```
Name: Max, Alter: 25, Beruf: Student
Name: Lisa, Alter: 30, Beruf: Lehrer
Name: Peter, Alter: 40, Beruf: Programmierer
```

```
// Beispiel 2: CSV-Datei mit variablen Spalten einlesen

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class CSVReader {

    public static void main(String[] args) {

        // Name der CSV-Datei angeben
        String csvFile = "C:/Dateien/data.csv";

        // Neue Liste für die Daten erstellen
        List<String[]> data = new ArrayList<>();

        // Versuchen, die CSV-Datei zu lesen und die Daten in die Liste zu speichern
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {

            String line;
            while ((line = br.readLine()) != null) {

                // Zeile in einzelne Spalten aufteilen (bei variablen Spalten)
                String[] columns = line.split("(?<!\\\\),");

                // Daten in die Liste speichern
                data.add(columns);

            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        // Daten ausgeben
        for (String[] columns : data) {
            System.out.println("Name: " + columns[0] + ", Alter: " + columns[1] + ", Beruf: " + columns[2]);
        }

    }

}
```

Beispiel-CSV-Datei "data.csv":

```
"Erika,36,"Programmierer, Künstler",München
"Tom,45,"Verkäufer",Berlin
```

Ausgabe:

```
Name: Erika, Alter: 36, Beruf: "Programmierer, Künstler"
Name: Tom, Alter: 45, Beruf: "Verkäufer"
```

# Ausführliche Informationen

## Historischer Kontext
CSV geht zurück bis ins Jahr 1972, als es in England entwickelt wurde, um Daten zwischen verschiedenen Computeranwendungen zu übertragen. Seitdem hat es sich zu einem gebräuchlichen Standard für den Austausch von Daten zwischen Programmen entwickelt.

## Alternativen
Es gibt auch andere Datenformate, die ähnliche Funktionen wie CSV erfüllen, wie z.B. XML oder JSON. Diese sind jedoch in der Regel komplexer und können daher langsamer in der Verarbeitung sein.

## Implementierungsdetails
Beim Einlesen von CSV-Dateien müssen einige Dinge beachtet werden, wie z.B. das gewählte Trennzeichen oder die mögliche Existenz von Anführungszeichen in den Daten, die die Aufteilung in Spalten erschweren können. Es ist daher wichtig, sorgfältig zu testen und sicherzustellen, dass die Implementierung den Anforderungen entspricht.

# Weitere Informationen
- [Java API-Dokumentation für CSV](https://docs.oracle.com/javase/10/docs/api/java/util/Collections.html#method.summary)
- [CSV-Format-Spezifikation](https://tools.ietf.org/html/rfc4180)