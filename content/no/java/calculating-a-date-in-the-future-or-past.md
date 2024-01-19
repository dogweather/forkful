---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Java: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Beregning av en dato i fremtiden eller fortiden handler om å tilføye eller trekke dager, uker, måneder eller år fra en eksisterende dato. Programmerere gjør dette for å utføre oppgaver som planlegging, aldersverifisering og påminnelser.

## Hvordan:
Her er et eksempel på hvordan man kan beregne en dato én uke i fremtiden i Java.

```Java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        LocalDate weekLater = today.plusDays(7);
        System.out.println("Dato en uke senere: " + weekLater);
    }
}
```
På kjøring, vil dette gi følgende output:

```
Dato en uke senere: yyyy-mm-dd
```
(hvor 'yyyy-mm-dd' vil være den faktiske datoen en uke fra nå)

## Dypdykk
Beregning av fremtidige eller tidligere datoer har vært en del av JDK siden Java 1.1, men dagens API (introdsuert i Java 8) er mer brukervennlig og lesbar. Et alternativ til ovennevnte metode er bruk av `java.util.Calendar`, men dette er mer klønete. Når det gjelder implementasjonsdetaljer, så håndterer `plusDays` funksjonen alle kompleksiteter med varierende månedslengde og skuddår.

## Se Også
For mer informasjon, vennligst se:
- [Oracle’s Java docs for LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Oracle’s Java docs for Period](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html)
- [Baeldung's guide to Java 8 Date and Time APIs](https://www.baeldung.com/java-8-date-time-intro).