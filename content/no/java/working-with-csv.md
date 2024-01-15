---
title:                "Arbeid med csv"
html_title:           "Java: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

CSV (Comma Separated Values) er et populært format for å lagre og dele data i en tabellstruktur. Derfor er det viktig å kunne arbeide med CSV-filer når du jobber med data, spesielt i Java-programmering. Det tillater deg å enkelt behandle og manipulere store mengder data og utveksle informasjon med andre programmer.

## Slik gjør du det

For å arbeide med CSV-filer i Java, må du først importere "java.io" og "java.util" pakker. Deretter må du initialisere FileReader og BufferedReader objekter for å lese filen, og String og String array variabler for å lagre dataene. Du kan deretter bruke en while-løkke for å lese linje for linje, og splitte hver linje basert på separator tegnet (vanligvis komma). Her er et eksempel på hvordan koden vil se ut:

```java
import java.io.FileReader;
import java.io.BufferedReader;
import java.util.Scanner;

public class CSVReader {
    public static void main(String[] args) {
        String fileName = "data.csv";
        String[] data;
        try {
            FileReader reader = new FileReader(fileName);
            BufferedReader bufferedReader = new BufferedReader(reader);
            
            String line = "";
            while ((line = bufferedReader.readLine()) != null) {
                data = line.split(",");
                for (String value : data) {
                    System.out.println(value);
                }
            }
            
            bufferedReader.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

La oss si at vi har en CSV-fil kalt "data.csv" som inneholder følgende informasjon:

```csv
Navn,Alder,Stilling
Petter,30,Ingeniør
Lena,25,Advokat
Ole,35,Lærer
```

Kjøring av koden vil gi følgende utskrift:

```
Navn
Alder
Stilling
Petter
30
Ingeniør
Lena
25
Advokat
Ole
35
Lærer
```

Dette eksemplet viser hvordan du enkelt kan lese og prosessere data fra en CSV-fil i Java.

## Dypdykk

Når du arbeider med CSV-filer, er det viktig å være oppmerksom på at separator tegnet kan variere fra fil til fil. Vanligvis er det enten komma, kolon eller semikolon, men det kan også være andre tegn som er brukt. Derfor kan det være lurt å bruke et Scanner objekt for å lese dataene og bruke Scanner delimiter metoden til å sette den riktige separator tegnet.

I tillegg bør du alltid håndtere eventuelle feil eller unntak som kan oppstå under lesing av en CSV-fil, som vist i eksempelet ovenfor. Dette sikrer at programmet ditt ikke feiler og kan håndtere uventede situasjoner.

## Se også

- Java Official Documentation: https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html
- Tutorialspoint: https://www.tutorialspoint.com/java/io/java_io_filewriter.htm
- Baeldung: https://www.baeldung.com/java-buffered-reader