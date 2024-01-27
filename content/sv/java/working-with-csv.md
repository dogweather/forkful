---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV, eller Comma-Separated Values, är ett enkelt filformat för att lagra tabelldata. Programmerare använder CSV för att enkelt utbyta data mellan olika program, system och språk.

## Hur gör man:
Java har inget inbyggt stöd för CSV, så vi använder ofta bibliotek som Apache Commons CSV eller OpenCSV. Här är ett exempel med Apache Commons CSV för att läsa och skriva CSV:

```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.csv.CSVRecord;

import java.io.Reader;
import java.io.Writer;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class CSVExempel {

    public static void lasaCSV() throws IOException {
        Reader in = new FileReader("exempel.csv");
        Iterable<CSVRecord> records = CSVFormat.DEFAULT.parse(in);
        for (CSVRecord record : records) {
            String kolumnEtt = record.get(0);
            String kolumnTva = record.get(1);
            // Gör något med data
        }
    }

    public static void skrivaCSV(List<String[]> data) throws IOException {
        Writer out = new FileWriter("nyExempel.csv");
        CSVPrinter printer = new CSVPrinter(out, CSVFormat.DEFAULT);
        for (String[] record : data) {
            printer.printRecord((Object[]) record);
        }
        printer.flush();
        printer.close();
    }
    
    public static void main(String[] args) throws IOException {
        lasaCSV();
        skrivaCSV(List.of(new String[]{"A1", "B1"}, new String[]{"A2", "B2"}));
    }
}
```
## Deep Dive
CSV-formatet har använts sedan 1970-talet och är ett smidigt sätt att lagra stora mängder data kompakt. Det saknar dock en standardiserad schema-definition, vilket kan leda till tolkningsproblem. Alternativ till CSV inkluderar JSON, XML och databaser. I Java kan bibliotek som Jackson eller Gson hantera JSON, medan JAX-B används för XML.

## Se Också
- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- OpenCSV: http://opencsv.sourceforge.net/
- Jackson: https://github.com/FasterXML/jackson
- Gson: https://github.com/google/gson
- JAX-B: https://javaee.github.io/jaxb-v2/
