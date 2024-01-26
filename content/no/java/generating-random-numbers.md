---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:15.667276-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall er essensielt for simuleringer, spill, sikkerhet, og hvor ellers uforutsigbarhet er nødvendig. Programmerere bruker det for å sikre variasjon og uforutsigbarhet i programmenes oppførsel.

## Hvordan:
For å generere et tilfeldig tall i Java, kan du bruke klassen `Random` eller `Math.random()` for enkel bruk, eller `SecureRandom` for kryptografisk sikre tall.

```java
import java.util.Random;

public class TilfeldigEksempel {
    public static void main(String[] args) {
        Random random = new Random();
        
        // Genererer et tilfeldig heltall mellom 0 (inkludert) og 100 (ekskludert)
        int tilfeldigTall = random.nextInt(100);
        System.out.println(tilfeldigTall);

        // Genererer et tilfeldig desimaltall mellom 0.0 og 1.0
        double tilfeldigDouble = Math.random();
        System.out.println(tilfeldigDouble);
    }
}
```
Eksempel på output:
```
42
0.7301587286504309
```

## Dypdykk:
I eldre versjoner av Java var `Random` klassen standard for å generere tilfeldige tall. Med introduksjonen av `ThreadLocalRandom` i Java 7, fikk vi et bedre alternativ for flertrådede applikasjoner. For situasjoner som krever høyere grad av sikkerhet, som i kryptografi, er `SecureRandom` det foretrukne valget siden den bruker mer forutsigbare kilder for entropi.

Alternativer eksisterer også utenfor standardbiblioteket, som Apache Commons Math-biblioteket, som tilbyr ytterligere funksjonalitet.

Implementasjonsdetaljer er viktige; `Random` bruker en lineær kongruensgenerator, mens `SecureRandom` vanligvis implementerer en entropidrevet algoritme som SHA1PRNG eller native operativsystemfunksjoner.

## Se Også:
- [Oracle Java Documentation: Class Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Oracle Java Documentation: Math.random()](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--)
- [Apache Commons Math User Guide](https://commons.apache.org/proper/commons-math/userguide/random.html)
- [Stack Overflow: When to use SecureRandom over Random](https://stackoverflow.com/questions/11051205/when-to-use-securerandom-over-random)
