---
title:                "Java: Utskrift av feilsøkingsmeldinger"
simple_title:         "Utskrift av feilsøkingsmeldinger"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/printing-debug-output.md"
---

{{< edit_this_page >}}

##Hvorfor
Java-programmering innebærer vanligvis å løse komplekse problemer og feilsøke koden din er en viktig del av prosessen. Å trykke debug-utgang er en effektiv måte å identifisere feil og forstå hvordan koden din kjører.

##Slik gjør du det
Debug-utgang kan enkelt oppnås ved å bruke Java's "System.out.println()" -funksjon. La oss si at vi har en enkel variabel definert som "int num = 5;" og vi ønsker å skrive ut verdien av variabelen vår. Dette kan gjøres ved å skrive "System.out.println(num);" og når vi kjører koden, vil outputet vårt være "5" i konsollen.

En annen nyttig funksjon er "System.out.printf()", som lar deg formatere utdataen i konsollen din. For eksempel kan du skrive "System.out.printf("Variabelen vår er %d", num);" og utdataen vil være "Variabelen vår er 5" i stedet for bare tallet 5.

```Java
int num = 5;

//Skriver ut verdien av variabelen
System.out.println(num);

//Formatterer utdatoen med printf
System.out.printf("Variabelen vår er %d", num);
```

##Dypdykk
Å trykke utdebug-utgang er spesielt nyttig når man ønsker å sjekke verdien av variabler og forstå hvordan koden din kjører gjennom ulike deler av programmet. Det er også nyttig når man ønsker å finne og fikse feil i koden.

En annen måte å bruke utdebug-utgang på er å legge til egendefinerte meldinger for å forstå hva som skjer i koden din. For eksempel kan du skrive "System.out.println("Øker variabelen vår med 1.");" for å vite når og hvor variabelen din øker med 1.

##Se også
- [Java's offisielle dokumentasjon om debugging](https://docs.oracle.com/javase/7/docs/technotes/guides/jpda/)
- [10 debugging tips for Java-utviklere](https://www.javacodegeeks.com/2017/09/10-debugging-tips-java-developers.html)
- [Feilsøking i Java - Feilsøking for nybegynnere](https://stackify.com/guide-java-debugging-tips/)