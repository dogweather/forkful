---
title:                "Läsning av en textfil"
html_title:           "Java: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa en textfil är en vanlig uppgift inom programmering och kan vara användbart när man vill läsa in data från en extern källa. Det kan vara användbart för att utföra operationer på den data som finns i filen eller för att visualisera den på ett annat sätt. 

## Så här gör du
För att läsa en textfil i Java används en FileReader och en BufferedReader. Det första steget är att skapa en FileReader som tar emot filnamnet som en parameter. Sedan skapar vi en BufferedReader som tar emot FileReader som parameter och använder metoden "readLine()" för att läsa varje rad i filen. Här är en kodexempel:

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class TextFileReader {
    public static void main(String[] args) {
        try {
            // Skapa en FileReader
            FileReader fileReader = new FileReader("textfil.txt");

            // Skapa en BufferedReader baserad på FileReader
            BufferedReader bufferedReader = new BufferedReader(fileReader);

            String line;

            // Läs varje rad i filen och skriv ut den
            while ((line = bufferedReader.readLine()) != null) {
                System.out.println(line);
            }

            // Stäng BufferedReader
            bufferedReader.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Om vi antar att vår textfil innehåller följande text:

```
Hej! Det här är en textfil.
Vi läser in den med hjälp av FileReader och BufferedReader.
```

Då kommer följande output att skrivas ut:

```
Hej! Det här är en textfil.
Vi läser in den med hjälp av FileReader och BufferedReader.
```

## Deep Dive
När vi läser en textfil med BufferedReader så delas filen in i flera rader som vi kan läsa en åt gången. Detta gör läsningen snabbare och mindre minneskrävande jämfört med att läsa hela filen på en gång. Du kan använda metoden "read()" om du vill läsa in en och enkel byte istället för en hel rad. Detta kan vara användbart om filen innehåller binär data. Du kan också använda "skip()" för att hoppa över ett visst antal tecken eller använda "mark()" och "reset()" för att komma tillbaka till en tidigare läsposition. Det finns också andra metoder som vi kan använda för att hantera specifika situationer när vi läser en textfil. Mer information om dessa finns i Java-dokumentationen.

## Se även
- Java FileReader och BufferedReader: https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html
- Java-dokumentationen för FileReader: https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html