---
title:                "Java: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

Das Arbeiten mit CSV-Dateien ist für viele Programmierer eine gängige Aufgabe, da es oft als Datenspeicherformat verwendet wird. Es ist wichtig zu wissen, wie man CSV-Dateien in Java verwenden kann, um Daten effizient zu speichern und zu verarbeiten.

## Wie macht man das?

Es gibt mehrere Möglichkeiten, CSV-Dateien in Java zu verwenden. Eine Möglichkeit ist die Verwendung einer externen Bibliothek wie Apache Commons CSV oder OpenCSV, die speziell für die Verarbeitung von CSV-Dateien entwickelt wurden. Eine andere Möglichkeit ist die Verwendung von Java-Klassen wie FileReader und BufferedReader, um die Datei zu lesen und sie dann manuell zu analysieren und zu verarbeiten.

### Beispiel 1: Verwendung von Apache Commons CSV

Um Apache Commons CSV zu verwenden, müssen Sie zunächst die Bibliothek zu Ihrem Projekt hinzufügen. In Eclipse können Sie dies tun, indem Sie mit der rechten Maustaste auf Ihr Projekt klicken, auf "Eigenschaften" gehen und dann unter "Java Build Path" die Bibliothek hinzufügen.

Sobald die Bibliothek hinzugefügt wurde, können Sie sie verwenden, um eine CSV-Datei zu lesen und die Daten in eine Liste zu speichern. Hier ist ein Beispielcode, der eine CSV-Datei mit den Spalten "Name" und "Alter" liest und die Daten in einer Liste von Person-Objekten speichert.

```Java
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

public class CSVReader {

    public static void main(String[] args) throws IOException {

        // Pfad zur CSV-Datei
        String path = "pfad/zur/datei.csv";

        // Liste zum Speichern der Daten
        ArrayList<Person> personenListe = new ArrayList<>();

        // CSV-Parser mit angegebenem Format erstellen
        CSVParser parser = CSVFormat.DEFAULT.withFirstRecordAsHeader().parse(new FileReader(path));

        // Durch alle Datensätze in der Datei iterieren
        for (CSVRecord record : parser) {
            // Daten aus dem Datensatz auslesen
            String name = record.get("Name");
            int alter = Integer.parseInt(record.get("Alter"));

            // Neues Person-Objekt erstellen und zur Liste hinzufügen
            Person person = new Person(name, alter);
            personenListe.add(person);
        }

        // Personendaten ausgeben
        for (Person person : personenListe) {
            System.out.println(person.getName() + ", " + person.getAlter());
        }
    }
}
```

### Beispiel 2: Verwendung von Java-Klassen

Um CSV-Dateien mit Java-Klassen wie FileReader und BufferedReader zu verwenden, müssen Sie die Datei Zeile für Zeile lesen und die Trennzeichen (in der Regel ein Komma) verwenden, um die Daten zu trennen und zu verarbeiten. Hier ist ein Beispielcode, der dieselbe CSV-Datei wie im ersten Beispiel liest und die Daten in einer Liste von Person-Objekten speichert.

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

public class CSVReader {

    public static void main(String[] args) throws IOException {

        // Pfad zur CSV-Datei
        String path = "pfad/zur/datei.csv";

        // Liste zum Speichern der Daten
        ArrayList<Person> personenListe = new ArrayList<>();

        // FileReader und BufferedReader zum Lesen der Datei verwenden
        FileReader fileReader = new FileReader(path);
        BufferedReader bufferedReader = new BufferedReader(fileReader);

        // Überschrift überspringen
        bufferedReader.readLine();

        // Durch jede Zeile in der Datei iterieren
        String line;
        while ((line = bufferedReader.readLine()) != null) {
            // Daten in der Zeile anhand des Trennzeichens trennen
            String[] data = line.split(",");

            // Daten auslesen
            String name = data[0];
            int alter = Integer.parseInt(data[1]);

            // Neues Person-Objekt erstellen und zur Liste hinzufügen
            Person person = new Person(name, alter);
            personenListe.add(person);
        }

        // Personendaten ausgeben
        for (Person person : personenListe) {
            System.out.println(person.getName() + ", " + person.getAlter());
        }

        // BufferedReader schließen
        bufferedReader.close();
    }
}