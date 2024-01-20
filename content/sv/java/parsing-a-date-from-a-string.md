---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att parsa ett datum från en sträng innebär att omvandla en textrepresentation av ett datum till ett programmerbart object. Programmerare gör detta för att kunna hantera och manipulera datumvärden i sina kodrutiner.

## Hur gör man:

Här är ett grundläggande exempel:
```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
  public static void main(String[] args) {
    String date = "2023-04-01";
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    
    LocalDate parsedDate = LocalDate.parse(date, formatter);
    System.out.println(parsedDate); // 2023-04-01
  }
}
```
I koden ovan skapar vi en DateFormatter med det önskade datummönstret ('yyyy-MM-dd') och använder sedan `LocalDate.parse()` för att konvertera en sträng till ett LocalDate-objekt. Koden printar ut date-objektet som en sträng.

## Djupdykning

Historiska kontext: I tidiga versioner av Java användes `SimpleDateFormat`-klassen för att parsa datumsträngar. Modernare versioner har dock infört `DateTimeFormatter`, som är mer trådsäker och flexibel.

Alternativ: Java tillåter också parsning av datumsträngar med hjälp av `Instant.Parse()` eller `ZonedDateTime.Parse()` beroende på användarens behov.

Implementeringsdetaljer: Funktionen `parsec()` är inte begränsad till förinställda format. Användare kan ange sitt eget format i `DateTimeFormatter`.

## Se också:

1. [DateTimeFormatter dokumentation](http://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
2. [Tutorial på Oracle](https://docs.oracle.com/javase/tutorial/datetime/iso/format.html)