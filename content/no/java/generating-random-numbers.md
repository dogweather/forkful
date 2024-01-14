---
title:    "Java: Generering av tilfeldige tall"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en viktig del av mange programmeringsoppgaver, enten det er for å sikre datasikkerheten eller for å legge til variasjon i et spill. Ved å forstå hvordan man genererer tilfeldige tall i Java, kan du utvide dine programmeringsferdigheter og åpne dørene for nye muligheter.

## Hvordan lage tilfeldige tall i Java

For å generere tilfeldige tall i Java kan du bruke Random-klassen. Her er et enkelt eksempel på hvordan du kan bruke denne klassen:

```java
import java.util.Random;

// Oppretter et nytt Random-objekt
Random random = new Random();

// Genererer et tilfeldig tall mellom 1 og 100 og lagrer det i en variabel
int randomNumber = random.nextInt(100) + 1;

// Printer ut det tilfeldige tallet
System.out.println("Det tilfeldige tallet er: " + randomNumber);
```

Dette vil skrive ut et tilfeldig tall mellom 1 og 100 hver gang du kjører koden. Du kan også bruke Random-klassen til å generere tilfeldige tall av andre datatyper, som for eksempel flyttall eller boolean.

## Dykk dypere ned i tilfeldige tall

Når vi snakker om tilfeldige tall, er det viktig å forstå at de ikke er helt tilfeldige. De blir generert av en algoritme og er basert på et såkalt "seed"-nummer. Dette betyr at hvis du bruker samme seed-nummer, vil den samme sekvensen av tilfeldige tall bli generert hver gang. Du kan også sette et eget seed-nummer for å få en spesifikk rekkefølge av tilfeldige tall.

En annen ting å være oppmerksom på er at Random-klassen ikke er thread-safe, noe som betyr at den ikke kan brukes i flertrådede applikasjoner. For å unngå problemer med dette, kan du bruke ThreadLocalRandom-klassen i stedet.

## Se også

- Java Random-klassen dokumentasjon: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- Tutorialspoint-siden om tilfeldige tall i Java: https://www.tutorialspoint.com/java/java_random_class.htm
- Hvordan generere unike tilfeldige tall i Java: https://www.baeldung.com/java-random-numbers

Tilfeldige tall er et viktig konsept å forstå for å kunne skrive mer komplekse og varierte programmer i Java. Med kunnskapen du har lært her, kan du nå utforske flere muligheter og finne ut hvordan du kan implementere det i dine egne programmer. Lykke til!