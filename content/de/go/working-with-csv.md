---
title:                "Go: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV (Comma Separated Values) ist ein häufig verwendetes Dateiformat für Tabellendaten. Es ist einfach zu lesen, zu schreiben und zu bearbeiten, was es zu einem beliebten Format für den Austausch von Daten macht. In diesem Blogbeitrag werden wir uns ansehen, wie wir mit CSV-Dateien in Go programmieren können.

## Wie geht's

Um mit CSV-Dateien in Go zu arbeiten, benötigen wir zuerst das Paket "encoding/csv". Dann können wir eine Datei öffnen und sie mithilfe des csv.Reader-Objekts analysieren. In folgendem Beispiel gehen wir davon aus, dass wir eine CSV-Datei mit folgenden Inhalten haben:

```Go
Name, Alter, Land
Max, 25, Deutschland
Sara, 30, Österreich
John, 28, USA
```
Wir können diese Datei mit dem folgenden Code öffnen und analysieren:

```Go
file, err := os.Open("example.csv")
if err != nil {
  log.Fatal(err)
}
defer file.Close() // wichtig, um sicherzustellen, dass die Datei nach Abschluss wieder geschlossen wird

reader := csv.NewReader(file)

records, err := reader.ReadAll()
if err != nil {
  log.Fatal(err)
}

for _, row := range records {
  name := row[0]
  age := row[1]
  country := row[2]

  fmt.Printf("%s ist %s Jahre alt und kommt aus %s\n", name, age, country)
}
```

Die Ausgabe würde wie folgt aussehen:

```
Max ist 25 Jahre alt und kommt aus Deutschland
Sara ist 30 Jahre alt und kommt aus Österreich
John ist 28 Jahre alt und kommt aus USA
```

Wir können auch einzelne Zeilen lesen, indem wir die Methode "Read" des csv.Reader-Objekts verwenden und die Methode "ReadAll" weglassen.

## Tiefer in das Thema eintauchen

Beim Arbeiten mit CSV-Dateien müssen wir möglicherweise die Daten in bestimmte Typen konvertieren, z.B. in Zahlen oder Datumsformate. Dafür bietet das "encoding/csv" Paket die Methode "strconv" an. Hier ist ein Beispiel für das Konvertieren von Alter in Ganzzahlen:

```Go
for _, row := range records {
  name := row[0]
  age, _ := strconv.ParseInt(row[1], 10, 64)
  country := row[2]

  fmt.Printf("%s ist %d Jahre alt und kommt aus %s\n", name, age, country)
}
```

Außerdem können wir auch CSV-Dateien schreiben, indem wir die Methode "Write" des csv.Writer-Objekts verwenden.

## Siehe auch

Weitere Informationen über das Arbeiten mit CSV-Dateien in Go finden Sie hier:

- [Offizielle Dokumentation zum "encoding/csv" Paket](https://golang.org/pkg/encoding/csv/)
- [CSV-Dateien mit Go lesen und schreiben](https://gobyexample.com/reading-files)

Schauen Sie sich auch andere nützliche Go-Pakete an, die bei der Arbeit mit CSV-Dateien helfen können:

- [Go-Rüstzeug für das CSV-Format](https://github.com/gocarina/gocsv)
- [CSV-Dateien in Go effizient lesen](https://github.com/jszwec/csvutil)