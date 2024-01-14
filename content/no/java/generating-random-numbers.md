---
title:                "Java: Fremstilling av tilfeldige tall"
simple_title:         "Fremstilling av tilfeldige tall"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor
Random nummergenerering er en viktig del av mange programmeringsoppgaver. Det kan hjelpe med å simulere tilfeldige situasjoner, generere unike IDer, og kan være nyttig for å teste funksjonalitet i et program. 

# Hvordan 
For å generere tilfeldige tall i Java, kan du bruke `Math.random()` funksjonen. Denne funksjonen returnerer et flyttall mellom 0.0 og 1.0. For å få et tilfeldig tall innenfor et bestemt område, kan du multiplisere resultatet med områdets størrelse og legge til startverdien. For eksempel, hvis du ønsker et tilfeldig tall mellom 1 og 10, kan du bruke følgende kode: 

```Java 
int min = 1; 
int max = 10; 
int random = (int)(Math.random() * (max - min + 1) + min); 
System.out.println(random); // Output: et tilfeldig tall mellom 1 og 10 
```

# Dypdykk 
Mens `Math.random()` er den mest vanlige måten å generere tilfeldige tall i Java, er det også andre metoder som gir mer kontroll over tilfeldigheten. Du kan for eksempel bruke `Random` klassen som lar deg spesifisere et såkalt "seed" - nummer for å generere en sekvens av tall. En seed-nummer brukes som utgangspunkt for å generere tilfeldige tall, og å bruke samme seed-nummer vil resultere i samme sekvens av tall. Dette kan være nyttig for å teste og feilsøke tilfeldighetsbasert funksjonalitet i et program. 

# Se også 
- [Offisiell Java dokumentasjon for Random klassen](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Tutorialspoint - Random number generation in Java](https://www.tutorialspoint.com/generating-random-numbers-in-java)
- [Java random number generators - en sammenligning](https://www.baeldung.com/java-random)