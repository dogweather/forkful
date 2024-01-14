---
title:                "Java: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV (Comma Separated Values) är ett populärt format för att lagra och dela tabellbaserad data. Genom att arbeta med CSV-filer kan vi enkelt organisera och manipulera stora mängder data. I denna bloggpost kommer vi att utforska hur man kan arbeta med CSV-filer i Java.

## Så här

För att hantera CSV-filer i Java behöver vi använda oss av en tredjepartsbiblioteket som heter "OpenCSV". Vi kan ladda ned biblioteket från deras hemsida eller använda Maven för att inkludera det i vårt projekt. Efter att ha inkluderat biblioteket kan vi använda klassen "CSVReader" för att läsa in en CSV-fil och spara den i en Array eller en List.

```Java
// Importera nödvändiga paket
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

import com.opencsv.CSVReader;

public class CSVExample {

    public static void main(String[] args) {
        // Läs in CSV-filen med hjälp av CSVReader
        try (CSVReader reader = new CSVReader(new FileReader("example.csv"))) {
            // Använda readAll() för att läsa in hela filen i en lista
            List<String[]> data = reader.readAll();
            // Loopa igenom listan och skriv ut varje rad
            for (String[] row : data) {
                System.out.println(Arrays.toString(row));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
#### Exempeloutput:
```
["Namn", "Ålder", "Land"]
["Lisa", "32", "Sverige"]
["John", "45", "USA"]
["Anna", "23", "Tyskland"]
```


Genom att använda oss av olika metoder i "CSVReader" kan vi också filtrera, sortera och manipulera data innan vi sparar den i en lista eller Array.

## Djupdykning

CSV-filer är vanligtvis enkelt strukturerade och innehåller endast text och numeriska värden. Detta gör det enkelt för oss att läsa in och bearbeta data utan att behöva hantera komplexa datastrukturer. Det är också ett mycket populärt format för att exportera och importera data mellan olika program och system.

När vi arbetar med CSV i Java är det viktigt att komma ihåg att hantera eventuella specialtecken eller teckenkodningar, särskilt om datafilen innehåller flera språk eller specialtecken. Det är också viktigt att alltid stänga "CSVReader" efter användning för att undvika läckor eller felaktig hantering av filressurser.

## Se också

- [OpenCSV hemsida](http://opencsv.sourceforge.net/)
- [Apache Commons CSV dokumentation](https://commons.apache.org/proper/commons-csv/index.html)