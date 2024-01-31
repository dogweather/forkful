---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-csv.md"
---

{{< edit_this_page >}}

# What & Why?
CSV (Comma-Separated Values) ist ein einfaches Format für strukturierte Daten. Programmierer nutzen es zum Speichern und Austauschen von Daten, da es leicht lesbar und zu bearbeiten ist.

# How to:
Java bietet keine Standardbibliothek für CSV, aber wir können sowohl selbst lesen als auch schreiben:

```java
import java.io.*; // Importe für IO-Operationen

public class CsvBeispiel {

    public static void main(String[] args) throws IOException {
        schreibeCsv("beispiel.csv");
        leseCsv("beispiel.csv");
    }

    private static void schreibeCsv(String dateiName) throws IOException {
        try (PrintWriter writer = new PrintWriter(new File(dateiName))) {
            StringBuilder sb = new StringBuilder();
            sb.append("Name");
            sb.append(',');
            sb.append("Alter");
            sb.append('\n');
            sb.append("Max Mustermann");
            sb.append(',');
            sb.append("42");
            sb.append('\n');
            
            writer.write(sb.toString());
        }
    }

    private static void leseCsv(String dateiName) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(dateiName))) {
            String zeile;
            while ((zeile = br.readLine()) != null) {
                String[] werte = zeile.split(",");
                System.out.println("Name: " + werte[0] + ", Alter: " + werte[1]);
            }
        }
    }
}
```

Sample Output:
```
Name: Max Mustermann, Alter: 42
```

# Deep Dive
CSV wurde in den 1970er Jahren populär. Es gibt viele Alternativen wie XML oder JSON, die komplexere Datenstrukturen unterstützen. CSV bleibt aber wegen seiner Simplizität beliebt. Beim Umgang mit CSV in Java sollte man besonders auf korrekten Umgang mit Zeichenkodierung und Escape-Regeln achten sowie bufferen beim Lesen und Schreiben verwenden, um Performance zu optimieren.

# See Also
- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- OpenCSV: http://opencsv.sourceforge.net/
- Java API for JSON Processing: https://javaee.github.io/jsonp/
