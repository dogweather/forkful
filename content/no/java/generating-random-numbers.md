---
title:                "Generering av tilfeldige tall"
html_title:           "Java: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Tilfeldige tall er en viktig del av programmering, spesielt for spill, simuleringer og sikkerhet. Å generere tilfeldige tall kan også være nyttig for å lage randomiserte tester og for å tilføre variasjon i et program.

## Hvordan

For å generere tilfeldige tall i Java, kan vi bruke Math.random() funksjonen. Denne funksjonen returnerer et tilfeldig tall mellom 0.0 og 1.0. Vi kan også bruke Random klassen for å generere tilfeldige tall med mer spesifikke krav.

```java
// Eksempel på å generere et tilfeldig tall mellom 0 og 100
double rand = Math.random() * 100; 
System.out.println(rand); // (f.eks. 72.846392)

// Eksempel på å generere et tilfeldig heltall mellom 1 og 10
Random random = new Random();
int randInt = random.nextInt(10) + 1; 
System.out.println(randInt); // (f.eks. 7)
```

## Dypdykk

Det er viktig å merke seg at tilfeldige tall som genereres av datamaskiner ikke er helt tilfeldige. De følger en algoritme som baserer seg på en startverdi kalt "seed". Hvis vi bruker samme seed hver gang, vil vi få samme sekvens av tilfeldige tall. For å unngå dette, kan vi bruke System.currentTimeMillis() som seed for å få tilfeldige tall basert på klokkeslettet.

Vi kan også bruke metoder som shuffle() og Collections.shuffle() for å få tilfeldig rekkefølge på elementer i en liste eller et array.

## Se også

- Oracle Java Documentation: https://docs.oracle.com/en/java/
- Random klassen dokumentasjon: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html
- Å generere tilfeldige tall i Java: https://www.javatpoint.com/generating-random-number-in-java