---
title:                "Arbeiten mit CSV"
html_title:           "C#: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

Sicherlich ist es schon vorgekommen, dass du mit großen Mengen von Daten arbeiten musstest. CSV-Dateien sind dabei ein häufig genutztes Format, um strukturierte Daten zu speichern. Mit C# kannst du CSV-Dateien ganz einfach einlesen, bearbeiten und speichern.

## Wie es geht

Um mit CSV-Dateien in C# zu arbeiten, benötigst du die `CsvHelper`-Bibliothek. Diese kannst du entweder manuell herunterladen und in dein Projekt einbinden oder über NuGet installieren.

Als Erstes musst du eine Klasse definieren, die die Struktur deiner CSV-Datei repräsentiert. Die Eigenschaften dieser Klasse müssen dabei den Spaltennamen in der CSV-Datei entsprechen. Zum Beispiel:

```C#
public class Person
{
    public string Vorname { get; set; }
    public string Nachname { get; set; }
    public int Alter { get; set; }
}
```

Um eine CSV-Datei einzulesen, erstellst du einen `CsvReader` und gibst den Dateipfad als Argument an. Dann liest du die Zeilen der Datei nacheinander aus und wandelst sie in Objekte der definierten Klasse um. Hier ein Beispiel:

```C#
using (var reader = new CsvReader(new StreamReader("meine-datei.csv")))
{
    while (reader.Read())
    {
        var person = reader.GetRecord<Person>();

        // hier kannst du mit den Daten der Person arbeiten
        Console.WriteLine($"{person.Vorname} {person.Nachname} ({person.Alter})");
    }
}
```

Du kannst aber auch selbst bestimmen, wie die Daten aus der CSV-Datei in dein Objekt umgewandelt werden sollen. Dafür kannst du sogenannte "mappings" definieren, die festlegen, wie die Spalten in der CSV-Datei mit den Eigenschaften deiner Klasse verknüpft werden sollen. Zum Beispiel:

```C#
// Spalte "Vorname" wird mit der Property "FirstName" gemappt
Map(m => m.FirstName).Name("Vorname");
```

Um eine CSV-Datei zu erstellen, erstellst du einen `CsvWriter` und übergibst als Argument das Ziel der Datei. Dann schreibst du einfach deine Daten in die Datei. Hier ein Beispiel:

```C#
using (var writer = new CsvWriter(new StreamWriter("neue-datei.csv")))
{
    // Definiere, wie die Spalten heißen sollen
    writer.WriteHeader<Person>();

    // Schreibe Objekte in CSV-Datei
    writer.WriteRecords(new List<Person>
    {
        new Person { Vorname = "Max", Nachname = "Mustermann", Alter = 28 },
        new Person { Vorname = "Maria", Nachname = "Musterfrau", Alter = 32 }
    });
}
```

In der erzeugten CSV-Datei hätten wir dann folgende Inhalte:

```
Vorname,Nachname,Alter
Max,Mustermann,28
Maria,Musterfrau,32
```

## Deep Dive

Wenn du tiefer in das Thema einsteigen möchtest, bietet die `CsvHelper`-Bibliothek noch viele weitere Funktionen. Zum Beispiel kannst du Datentypen konvertieren lassen, Zeilen ignorieren oder manipulieren und Header-Namen anpassen.

Zusätzlich ist es wichtig, dass die Werte in deiner CSV-Datei richtig formatiert sind. Wenn du zum Beispiel Texte hast, die Kommas enthalten, musst du diese in Anführungszeichen setzen, damit sie korrekt eingelesen werden können. Auch hierfür bietet `CsvHelper` eine Lösung, indem es automatisch die Anführungszeichen hinzufügt.

## Siehe auch

- [CsvHelper-Dokumentation](https://joshclose.github.io/CsvHelper/) 
- [CSV-Dateien einlesen und schreiben mit C#](https://www.c-sharpcorner.com/article/read-and-write-csv-files-in-c-sharp/)