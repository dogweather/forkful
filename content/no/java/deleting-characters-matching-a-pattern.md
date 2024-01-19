---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Slette tegn som passer til et mønster, betyr at du fjerner bestemte symboler fra en streng ved hjelp av et bestemt filtreringsmønster. Programmerere gjør dette for å rense eller formatere data.

## Slik gjør du:
Java tilbyr `String.replaceAll(regex, replacement)` funksjonen for å slette tegn som passer til et mønster. Her er et enkelt eksempel:

```Java
public class Main {
    public static void main(String[] args) {
        String txt = "Hei verden123!";

        // Sletter alle tall i teksten
        String cleanTxt = txt.replaceAll("\\d", "");

        System.out.println(cleanTxt);
    }
}
```

Når du kjører programmet, vil det returnere følgende utskrift:

```Java
Hei verden!
```

## Dyp Dykk
Først og fremst, hele idéen om å slette tegn basert på et mønster stammer fra regulære uttrykk (regex), som er viktig i strengmanipulasjoner. Regulære uttrykk er kraftige, men er kanskje ikke alltid det mest effektive, spesielt i Java. 

Alternativt kan du bruke `StringBuilder`-klassen i Java, som er raskere når du har et stort datasett å manipulere. Her er et eksempel:

```Java
public class Main {
    public static void main(String[] args) {
        StringBuilder sb = new StringBuilder("Hei verden123!");
    
        for(int i = 0; i < sb.length(); i++){
            // hvis tegnet på indeks i er et tall, slett det.
            if(Character.isDigit(sb.charAt(i))){
                sb.deleteCharAt(i);
                i--; // justerer indeksen etter karakteren er slettet
            }
        }
    
        System.out.println(sb.toString());
    }
}
``` 

Når du kjører denne koden, vil du også få samme utskrift som "Hei verden!".

Begge metodene kan være gunstig avhengig av dine behov og datatyper.

## Se også
For mer informasjon om Java String-klassen, se [Java String-dokumentasjonen](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html).

For ytterligere detaljer og alternativer for å slette tegn fra en streng i Java, kan du besøke følgende [Stack Overflow tråd](https://stackoverflow.com/questions/5724644/remove-character-from-a-string-at-a-certain-position-java). 

For informasjon om Java StringBuilder-klassen, kan du følge [den offisielle Oracle-dokumentasjonen](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/StringBuilder.html).