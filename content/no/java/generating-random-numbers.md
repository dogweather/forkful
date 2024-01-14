---
title:    "Java: Generering av tilfeldige tall"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan lage tilfeldige tall i Java-programmer? Generering av tilfeldige tall kan være nyttig i en rekke programmeringsoppgaver, som for eksempel spill, simuleringer og kryptering. Ved å forstå hvordan du kan generere tilfeldige tall, kan du legge til en ekstra dimensjon i dine Java-prosjekter.

## Hvordan du kan generere tilfeldige tall i Java

Java har en innebygd klasse kalt Random som lar deg generere tilfeldige tall. Her er et enkelt eksempel på hvordan du kan bruke Random-klassen for å generere et tilfeldig tall mellom 1 og 10:

```Java
import java.util.Random;

public class RandomNumberGenerator {
    public static void main(String[] args) {
        Random random = new Random();
        int randomNumber = random.nextInt(10) + 1;
        System.out.println("Det tilfeldige tallet er: " + randomNumber);
    }
}
```

I dette eksemplet opprettes en Random-objekt, og deretter brukes metoden nextInt() til å generere et tilfeldig tall mellom 0 og 9. Ved å legge til 1 til resultatet, får vi et tilfeldig tall mellom 1 og 10. Du kan justere grensene etter dine behov, for eksempel hvis du vil ha et tilfeldig tall mellom 50 og 100, kan du bruke random.nextInt(50) + 50.

## Dykke dypere i generering av tilfeldige tall

Random-klassen i Java bruker en algoritme kalt pseudo-random number generator (PRNG) for å generere tilfeldige tall. Dette betyr at tallene som genereres ikke er helt tilfeldige, men følger et mønster basert på startverdien som er brukt. Derfor, hvis du starter med samme startverdi, vil du få den samme sekvensen av tilfeldige tall hver gang.

Hvis du vil ha en mer tilfeldig generering av tall, kan du bruke klassen SecureRandom i stedet for Random. Denne klassen bruker en sikker algoritme for å generere tilfeldige tall, noe som gjør det vanskeligere å forutsi tallsekvensen.

## Se også

- [Java Random Klasse Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java SecureRandom Klasse Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- [Java-generering av tilfeldige tall-tutorial](https://www.baeldung.com/java-random)