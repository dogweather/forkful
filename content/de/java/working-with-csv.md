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

## Warum
Warum sollte man sich mit CSV beschäftigen? Nun, CSV (Comma-Separated Values) ist ein gängiges Dateiformat für Tabellendaten und wird in vielen Anwendungen verwendet, wie z.B. in Microsoft Excel oder Google Sheets. Es ist wichtig zu verstehen, wie man CSV-Daten in Java lesen und schreiben kann, um effektiv mit diesen Dateien zu arbeiten.

## Wie geht's
Um CSV-Daten in Java zu lesen und zu schreiben, gibt es verschiedene Möglichkeiten. Eine einfache Methode ist die Verwendung der integrierten Klasse "BufferedReader", um die Datei Zeile für Zeile zu lesen oder zu schreiben. Folgendes Beispiel zeigt, wie man eine CSV-Datei liest und die Daten in der Konsole ausgibt:

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class CSVReader {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("beispiel.csv"))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] data = line.split(",");
                System.out.println("Name: " + data[0] + " | Alter: " + data[1]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Als Ausgabe erhalten wir die Inhalte der CSV-Datei in folgendem Format:
```
Name: Max | Alter: 25
Name: Anna | Alter: 30
Name: Tim | Alter: 28
```

Um eine CSV-Datei zu schreiben, können wir die Klasse "BufferedWriter" verwenden, die ähnlich wie "BufferedReader" funktioniert. Hier ist ein Beispiel, das Daten in eine CSV-Datei schreibt:

```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class CSVWriter {
    public static void main(String[] args) {
        try (BufferedWriter bw = new BufferedWriter(new FileWriter("neue_datei.csv"))) {
            bw.write("Max,25");
            bw.newLine();
            bw.write("Anna,30");
            bw.newLine();
            bw.write("Tim,28");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Dieses Beispiel schreibt die Daten in die Datei in der gleichen Formatierung, wie wir sie vorher gelesen haben.

## Tiefergehende Informationen
Beim Arbeiten mit CSV-Dateien ist es wichtig zu beachten, dass Werte in Anführungszeichen vorhanden sein können, um speziellen Zeichen und Trennzeichen zu enthalten. Um diese Zeichen zu behandeln, kann die Klasse "CSVReader" aus der Apache Commons CSV Bibliothek verwendet werden. Diese bietet zusätzliche Funktionen für das Lesen und Schreiben von CSV-Daten. Auch die Apache Commons CSV Bibliothek ist eine gute Wahl, wenn die CSV-Datei große Datenmengen beinhaltet, da sie effizienter als die oben genannten Beispiele ist.

## Siehe auch
- [Dokumentation zu BufferedReader in Java](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Dokumentation zu BufferedWriter in Java](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html)
- [Dokumentation zu CSVReader in Apache Commons CSV](https://commons.apache.org/proper/commons-csv/apidocs/org/apache/commons/csv/CSVReader.html)
- [Dokumentation zu CSVWriter in Apache Commons CSV](https://commons.apache.org/proper/commons-csv/apidocs/org/apache/commons/csv/CSVWriter.html)