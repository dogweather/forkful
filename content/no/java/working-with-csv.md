---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Arbeid med CSV handler om å lese og skrive data i kommaseparerte filer. Programmerere gjør dette fordi CSV er enkel å forstå, lett å lese, og universelt støttet over mange programmer og plattformer.

## How to:

For å jobbe med CSV i Java, kan vi bruke `java.util.Scanner` for enkel innlesing eller biblioteker som Apache Commons CSV for mer avanserte behov. Her er et grunnleggende eksempel:

```java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class SimpleCsvReader {
    public static void main(String[] args) throws FileNotFoundException {
        File csvFile = new File("data.csv");
        Scanner scanner = new Scanner(csvFile);
        
        while (scanner.hasNext()) {
            String[] data = scanner.nextLine().split(",");
            // Prosesser data her
            System.out.println("Første kolonne verdi: " + data[0]);
        }
        
        scanner.close();
    }
}
```

Forventet output:
```
Første kolonne verdi: Verdi1
Første kolonne verdi: Verdi2
```

## Deep Dive

CSV har vært et populært datautvekslingsformat siden 1970-tallet grunnet sin enkelhet. Alternativer som JSON eller XML tilbyr mer struktur og funksjoner, men CSV opprettholdes for kompatibilitet og brukervennlighet. Når det kommer til implementering, er det avgjørende å håndtere ulike skilletegn og tekstinnpakning, spesielt i internasjonale kontekster.

## See Also

- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- OpenCSV: http://opencsv.sourceforge.net/
- Java API for JSON Processing: https://javaee.github.io/jsonp/
