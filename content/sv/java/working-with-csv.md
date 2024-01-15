---
title:                "Arbeta med csv"
html_title:           "Java: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV (Comma Separated Values) är en vanlig filformat som används för att lagra och överföra data i en tabellform. Det är ett mycket användbart verktyg för att hantera stora datamängder, och det är också enkelt att arbeta med i Java-programmering.

## Hur man gör

För att använda CSV-filer i Java behöver du först importera biblioteket "java.io.*". Sedan kan du läsa och skriva till CSV-filer genom att följa dessa enkla steg:

1. Skapa en "Fil" objekt med sökvägen till din CSV-fil som argument.
2. Skapa en "Scanner" objekt för att läsa in datan från filen.
3. Använd en loop för att läsa in rad för rad från filen, och använda metoden "split()" för att dela upp datan baserat på skiljetecken (vanligtvis kommatecken i en CSV-fil).
4. Använda "split()" igen för att dela upp varje rad i enskilda dataelement.
5. Skapa en "FileWriter" objekt för att skriva till en CSV-fil.
6. Använd en loop för att skriva ut varje rad till filen, med rätt skiljetecken mellan varje dataelement.
7. Stäng filen när du är klar.

Ett enkelt exempel på hur du läser in och skriver till en CSV-fil kan se ut så här:

```Java
import java.io.*;
import java.util.*;

public class CsvExample {
    public static void main(String[] args) {
        // Skapa en Scanner för att läsa in från filen
        File file = new File("minfil.csv");
        Scanner scanner = new Scanner(file);

        // Skapa en FileWriter för att skriva till filen
        FileWriter writer = new FileWriter("nyfil.csv");

        // Läs in raderna från filen
        while (scanner.hasNextLine()) {
            // Dela upp raden baserat på kommatecken
            String[] data = scanner.nextLine().split(",");

            // Skriv ut varje dataelement med ett semikolon som skiljetecken
            for (String element : data) {
                writer.write(element + ";");
            }
            // Lägg till en ny rad i filen
            writer.write("\n");
        }

        // Stäng filerna
        scanner.close();
        writer.close();
    }
}
```

Exemplet ovan läser in en CSV-fil och skriver ut datan från filen till en ny CSV-fil med ett semikolon som skiljetecken mellan varje dataelement.

## Djupdykning

När man arbetar med CSV-filer i Java är det viktigt att vara medveten om några möjliga problem som kan uppstå. Ett vanligt problem är när datan innehåller skiljetecken, vilket kan leda till att datan tolkas felaktigt när den delas upp med "split()". Ett sätt att lösa detta är att använda en annan separator (t.ex. tabb eller kolontecken) eller att använda citationstecken runt datan som innehåller skiljetecken.

En annan sak att vara medveten om är att "split()" metoden returnerar en "String[]" array av datatyper. Om datan du arbetar med innehåller olika datatyper (t.ex. både strängar och numeriska värden), kan det vara bättre att använda en CSV-parser för att kunna hantera datan på ett mer flexibelt sätt.

Det finns flera tredjepartsbibliotek som gör det enklare att arbeta med CSV-filer i Java, exempelvis OpenCSV och Apache Commons CSV. Dessa bibliotek erbjuder mer avancerade funktioner som möjligheten att direkt mappa en CSV-fil till ett objekt i Java, eller att hantera felaktig formaterad data på ett mer effektivt sätt.

## Se även

- [OpenCSV](http://opencsv.sourceforge.net/)
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/)