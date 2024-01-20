---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en dato til en streng i Java innebærer å forandre en Date-objekt til en lesbar streng. Programmerere gjør dette for å gjøre det lettere for brukere å forstå og lese datoverdier.

## Hvordan:

Her er et eksempel på hvordan du konverterer en dato til en streng i Java:

```Java
import java.util.Date;
import java.text.SimpleDateFormat;

public class Main {
    public static void main(String[] args) {
        Date date = new Date();
        SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");

        String dateString = format.format(date);
        System.out.println(dateString);
    }
}
```
Når du kjører dette programmet, får du følgende utgang:

```Java
22-02-2022 11:30:00
```

## Dypdykk:

Java's `SimpleDateFormat` ble introdusert i JDK 1.1, og gir en måte å både parse og formatere datoverdier. I JDK 8 ble klassen `DateTimeFormatter` introdusert som en del av det nye dato/tid- APIet, som alternativ til `SimpleDateFormat`, for å tilveiebringe trådsikkerhet og ytelsesforbedringer.

```Java
DateTimeFormatter dtf = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");
LocalDateTime now = LocalDateTime.now();
```
I dette tilfellet, om svært nøyaktige tidspunkter (for eksempel i nanosekunder) er nødvendige, 'Instant'-klassen sammen med 'DateTimeFormatter' kan brukes.

## Se Også:

Følgende lenker gir mer sosial kontekst rundt Java datohåndtering:

2. [Java SimpleDateFormat](https://www.javatpoint.com/java-simpledateformat)
3. [Java Date and Time tutorial](https://www.w3schools.com/java/java_date.asp)
4. [DateTimeFormatter API documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/format/DateTimeFormatter.html)