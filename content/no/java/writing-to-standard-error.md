---
title:    "Java: Å skrive til standardfeil"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere føler seg kanskje litt skremt når de hører uttrykket "skrive til standard-error", men det er en viktig del av Java programmering og kan være nyttig i mange situasjoner. Det er en enkel måte å debugge og finne feil i koden din, og det er spesielt nyttig når du jobber med større prosjekter. I denne bloggposten vil vi gå gjennom hvorfor og hvordan man skriver til standard-error i Java.

## Hvordan

For å skrive til standard-error i Java, må vi bruke System.err.println() funksjonen. Dette vil skrive ut teksten som en feilmelding i konsollen, noe som gjør det enkelt å skille mellom "normale" utskrifter og feilmeldinger. La oss se på et enkelt eksempel:

```Java
System.err.println("Dette er en feilmelding.");
System.out.println("Dette er en vanlig utskrift.");

```

Output:
```
Dette er en feilmelding.
Dette er en vanlig utskrift.
```

Som du kan se, blir teksten som er skrevet til standard-error vist med røde bokstaver i konsollen, noe som gjør det lett å identifisere som en feilmelding. Dette er spesielt nyttig når man skal feilsøke og finne ut hvor feilene i koden ligger.

Det er også mulig å skrive til standard-error i try-catch klosser ved hjelp av e.printStackTrace() funksjonen. Dette vil skrive ut en fullstendig stakksporing av feilen, noe som ofte er veldig nyttig for å identifisere årsaken til feilen.

## Dypdykk

Nå som vi vet hvordan man skriver til standard-error, la oss dykke dypere inn i hvorfor dette kan være nyttig. Når man jobber med større prosjekter, kan koden bli veldig kompleks og det kan være vanskelig å finne ut hvor en feil kommer fra. Ved å skrive til standard-error, kan man plassere feilmeldinger på bestemte steder i koden for å identifisere hvilke deler som forårsaker problemer. Dette gjør det mye lettere å debugge og løse problemene.

Det er også viktig å vite at standard-error er beregnet på å håndtere feil i koden, ikke vanlig utdata. Derfor bør man bare bruke System.err.println() funksjonen når man har en faktisk feil i koden, og ikke som erstatning for vanlige utskrifter.

## Se også

- [Java - Exception Handling](https://www.w3schools.com/java/java_try_catch.asp)
- [Debugging in Java](https://www.baeldung.com/java-debugging)
- [System.err and System.out in Java](https://howtodoinjava.com/java/system-err-and-system-out-in-java/)