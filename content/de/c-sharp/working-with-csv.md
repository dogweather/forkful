---
title:                "In Zusammenarbeit mit CSV arbeiten"
html_title:           "C#: In Zusammenarbeit mit CSV arbeiten"
simple_title:         "In Zusammenarbeit mit CSV arbeiten"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV ist eine Abkürzung für Comma-Separated Values und ist ein weit verbreitetes Dateiformat für den Austausch von Tabellendaten. Programmierer nutzen CSV, um Daten aus verschiedenen Quellen in ein einheitliches Format zu bringen oder um Daten in Excel-Tabellen zu importieren und zu exportieren.

## So geht's:
Beim Lesen oder Schreiben von CSV-Dateien gibt es einige wichtige Schritte zu beachten. Zunächst müssen wir die ```System.IO```-Bibliothek importieren, um mit Dateien zu arbeiten. Dann können wir die Klasse ```CsvReader``` oder ```CsvWriter``` verwenden, je nachdem ob wir Daten lesen oder schreiben möchten. Ein Beispiel zum Lesen einer CSV-Datei könnte folgendermaßen aussehen:

```
string filepath = "C:\\example.csv";

using (var reader = new CsvReader(filepath))
{
    while (reader.Read())
    {
        string column1 = reader.GetField<string>(0);
        int column2 = reader.GetField<int>(1);
        // weitere Logik
    }
}
```

## Tief eintauchen:
CSV wurde bereits in den 1970er Jahren entwickelt, lange bevor Excel oder andere Tabellenkalkulationsprogramme existierten. Heutzutage gibt es verschiedene Alternativen wie JSON oder XML, die ebenfalls für den Datenaustausch verwendet werden können. Bei der Implementierung ist zu beachten, dass CSV keine standardisierte Spezifikation hat, was zu verschiedenen Interpretationen führen kann. Es ist daher wichtig, immer darauf zu achten, dass die Daten korrekt formatiert sind.

## Siehe auch:
- Offizielle Dokumentation zur CsvHelper-Bibliothek: https://joshclose.github.io/CsvHelper/
- Vergleich von CSV, JSON und XML: https://www.upwork.com/resources/csv-json-xml-comparison
- Spezifikation von CSV: https://tools.ietf.org/html/rfc4180