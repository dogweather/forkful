---
title:    "Java: In Norwegian: Stor bokstav på en streng"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#Hvorfor

Å kapitalisere en streng er en vanlig oppgave i programmering som kan være nyttig for å formatere data eller vise korrekt tekst i applikasjoner. Dette kan også være en del av en større oppgave, for eksempel å sortere data eller manipulere tekst på en spesifikk måte.

#Slik Gjør Du
```Java
public static String capitalize(String str) {
    if (str == null) {
        return null;
    }

    return Character.toUpperCase(str.charAt(0)) + str.substring(1);
}

public static void main(String[] args) {
    String str = "dette er en test";

    System.out.println(capitalize(str));
}
```
Output: Dette er en test

For å kapitalisere en streng i Java, kan du bruke metoden "capitalize()" som tar inn en streng som parameter og returnerer den kapitaliserte versjonen. Første bokstav i strengen blir gjort til stor bokstav ved å bruke metoden "toUpperCase()" fra Character-klassen, og deretter blir resten av strengen returnert ved å bruke "substring()" metoden.

#Dypdykk
Selv om dette eksempelet er en enkel måte å kapitalisere en streng på, er det viktig å forstå hvordan metoden fungerer. I koden sjekker vi først om strengen er null, hvis den er det, returnerer vi også null. Dette er en viktig sjekk for å unngå feil i koden. Deretter tar vi første bokstav i strengen og gjør den til en stor bokstav. For å få resten av strengen bruker vi "substring()" metoden som tar inn en start og sluttposisjon for å returnere en del av strengen. I dette tilfellet starter vi på posisjon 1 for å få resten av strengen.

Et annet viktig aspekt ved å kapitalisere strenger er å vurdere diakritiske tegn, som for eksempel æ, ø og å i det norske alfabetet. En mer robust metode for å kapitalisere strenger bør håndtere disse tegnene for å få riktig utgang.

#Se Også
- [String dokumentasjon fra Oracle](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Character dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html)
- [tutorialspoint.com - Java String capitalize() metode](https://www.tutorialspoint.com/java/java_string_capitalize.htm)