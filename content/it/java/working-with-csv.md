---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con CSV significa manipolare file delimitati da virgole, comuni per la loro semplicità e compatibilità. Programmatore li usa per importare, esportare, e manipolare dati in modo leggero e trasversale tra sistemi.

## How to:
Ecco un esempio di lettura di un file CSV e stampa del suo contenuto in Java:

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class LeggiCSV {
    public static void main(String[] args) {
        String path = "dati.csv";
        String line;
        
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                System.out.println("Colonna 1: " + values[0] + ", Colonna 2: " + values[1]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Output di esempio:
```
Colonna 1: nome, Colonna 2: età
Colonna 1: Luigi, Colonna 2: 34
Colonna 1: Maria, Colonna 2: 28
```

Per salvare dati in un file CSV:

```java
import java.io.FileWriter;
import java.io.IOException;

public class ScriviCSV {
    public static void main(String[] args) {
        String[] data = { "Mario", "30" };
        String csvFile = "output.csv";
        
        try (FileWriter fw = new FileWriter(csvFile)) {
            fw.append(String.join(",", data));
            fw.append("\n"); // Nuova linea dopo ogni riga di dati
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Contenuto `output.csv`:
```
Mario,30
```

## Deep Dive
I CSV, acronimo di Comma-Separated Values, sono usati dagli anni '70 per gestire grandi quantità di dati tabulari. Alternative includono JSON, XML, e database, ma CSV è spiccatamente più semplice per file di piccole dimensioni. Java non offre una libreria interna dedicata al CSV, quindi si usano stringhe e I/O per gestirlo. Librerie esterne come Apache Commons CSV o OpenCSV offrono parsing più robusto e funzionalità.

## See Also
- [OpenCSV](http://opencsv.sourceforge.net/)
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/)
- Oracle Tutorial su I/O: [link](https://docs.oracle.com/javase/tutorial/essential/io/)
- RFC 4180, la specifica formale per CSV: [link](https://tools.ietf.org/html/rfc4180)
