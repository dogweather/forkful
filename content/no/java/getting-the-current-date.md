---
title:                "Java: Å få gjeldende dato"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å ville få tak i dagens dato i et Java-program. Kanskje du ønsker å vise datoen til brukeren, eller kanskje du trenger å lagre den som en del av programmet ditt. Uansett, å vite hvordan du kan hente den nåværende datoen er en viktig ferdighet for enhver Java-programmerer.

## Slik gjør du det

Det finnes flere måter å få tak i dagens dato i Java, men den mest vanlige og enkleste er å bruke Java's `Date`-klasse og dens `getDate()`-metode. Her er en enkel kode som viser hvordan du kan bruke denne metoden for å få tak i dagens dato og skrive den ut til konsollen:

```Java
import java.util.Date;

public class DagensDato {
    public static void main(String[] args) {
        Date date = new Date(); // Oppretter et nytt Date-objekt som representerer nåværende dato og tid
        int dag = date.getDate(); // Henter ut datoen fra Date-objektet
        System.out.println("Dagens dato er: " + dag);
    }
}
```

Når du kjører dette programmet, vil du få følgende utskrift på konsollen:

```
Dagens dato er: 10
```

Som du ser, hentes datoen ut som et heltall. Hvis du ønsker å få tak i måned og år i tillegg, kan du bruke `getMonth()` og `getYear()` metoder på samme måte som `getDate()`.
Det finnes også andre nyttige klasser og metoder for å jobbe med datoer i Java, som `Calendar`-klassen og dens `getTime()`-metode. Utforske disse ville være en god øvelse for å lære mer om hvordan Java håndterer datoer på en effektiv og nøyaktig måte.

## Dypdykk

Når du bruker `Date`-klassen for å få tak i dagens dato, er det viktig å være klar over at denne klassen faktisk representerer en spesifikk dato og tidspunkt, ikke bare en dato. Det betyr at hvis du ønsker å få tak i dagens dato, vil du også få med tiden på dagen da du hentet den ut. Dette kan være nyttig i noen tilfeller, men hvis du bare trenger datoen, kan det være lurt å bruke `Calendar`-klassen i stedet for å unngå noen uønskede overraskelser.

## Se også

- [Oracle Java: Date Class](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Oracle Java: Calendar Class](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [GeeksforGeeks: Get Current Date and Time in Java](https://www.geeksforgeeks.org/get-current-date-and-time-in-java/)