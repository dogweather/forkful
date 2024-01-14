---
title:                "Java: Generering av tilfeldige tall"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Generering av tilfeldige tall er en viktig del av mange programmeringsoppgaver. Det kan være nyttig for å teste koden din, lage simuleringer eller for å tilføre tilfeldighet i et spill eller en annen applikasjon.

## Slik gjør du det

Det finnes mange måter å generere tilfeldige tall på i Java. Her er to eksempler som viser deg hvordan du kan gjøre det:

```Java
// Eksempel 1 - Bruk Random-klassen

// Importer Random-klassen
import java.util.Random; 

// Opprett et Random-objekt
Random rand = new Random();

// Generer et tilfeldig heltall mellom 0 og 10
int randomNumber = rand.nextInt(11);

// Print ut det tilfeldige tallet
System.out.println("Tilfeldig tall: " + randomNumber);


// Eksempel 2 - Bruk Math.random() metoden

// Generer et tilfeldig desimaltall mellom 0 og 1
double randomDecimal = Math.random();

// Gjør desimaltallet om til et heltall mellom 0 og 10
int randomInteger = (int) (randomDecimal * 10);

// Print ut det tilfeldige tallet
System.out.println("Tilfeldig tall: " + randomInteger);
```

## Dypdykk

Hvis du ønsker å lære mer om hvordan tilfeldige tall genereres i Java, kan du se nærmere på de forskjellige metodekallene og hvordan de fungerer. Klassen Random tilbyr ulike metoder for å generere ulike typer tilfeldige tall, som for eksempel flyttall, heltall eller booleanske verdier. Math-klassen har også en metode som returnerer et tilfeldig desimaltall mellom 0 og 1.

En viktig ting å huske på når du genererer tilfeldige tall i Java er å sette et seed-nummer. Dette seed-nummeret bestemmer startverdien for genereringen av tilfeldige tall, og ved å bruke samme seed-nummer vil du få samme rekkefølge av tilfeldige tall hver gang koden kjøres.

## Se også

- [Java Random Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Math Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- [Java Random Numbers Tutorial](https://www.javatpoint.com/java-random-number)