---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generering av tilfeldige tall i Java

## Hva & Hvorfor?
Generering av tilfeldige tall er prosessen med å produsere tall som ikke har noe mønster. Programvareutviklere gjør dette for å simulere usikkerhet i funksjonalitet som spill, simuleringer eller testing.

## Hvordan:
Eller enkelhet, vi kan bruke `java.util.Random` eller `java.util.concurrent.ThreadLocalRandom` for enkel trådsikkerhet. Her er noen eksempler:

```Java
import java.util.Random;

public class Test {
    
    public static void main(String[] args) {
        Random rand = new Random();

        int tilfeldigTall = rand.nextInt(100);
        System.out.println(tilfeldigTall);
    }
}
```
output:
```
37
```
Trådsikker eksempel:
```Java
import java.util.concurrent.ThreadLocalRandom;

public class Test {
    
    public static void main(String[] args) {
        int tilfeldigTall = ThreadLocalRandom.current().nextInt(100);
        System.out.println(tilfeldigTall);
    }
}
```
output:
```
54
```

## Dypdykk
Generering av tilfeldige tall har en lang historie som strekker seg tilbake til antikken, men i moderne programmering, det kom til liv i midten av 20. århundre når datamaskinen ble skapt. 

Alternativene til metodene over inkluderer `java.security.SecureRandom` for kryptografisk sikre tall, og `java.util.stream` for strøm av tilfeldige tall.

Intern for `Random` og `ThreadLocalRandom` bruker de en lineær kongruensmetode. Først initialiserer de en frøverdi. Deretter, med hver anrop til `nextInt()`, `nextFloat()`, etc., regner de ut neste frø og returnerer deler av det som det neste tilfeldige tallet.

## Se også
For mer informasjon, er her noen gode kilder:
- Java Dokumentasjon på `Random`: [link](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/Random.html)
- Artikkel på Baeldung om generering av tilfeldige tall i Java: [link](https://www.baeldung.com/java-generate-random-long-float-integer-double)
- StackOverflow tråd om `Random` vs `ThreadLocalRandom`: [link](https://stackoverflow.com/questions/363681/how-do-i-generate-random-integers-within-a-specific-range-in-java).