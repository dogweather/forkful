---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga nummer i programmering används för att skapa en osäkerhet, vilket är användbart i många applikationer som spel, simuleringar och kryptering.

## Hur man gör:
Använd klassen `java.util.Random` i Java för att generera slumpmässiga nummer. Här är ett grundläggande exempel:

```Java
import java.util.Random;

public class Main {
  public static void main(String[] args) {
    Random rand = new Random();
    
    int randomNum = rand.nextInt(50);
    System.out.println("Slumpmässigt nummer: " + randomNum);
  }
}
```
När du kör detta program, kommer det att skriva ut ett slumpmässigt nummer mellan 0 och 49.

## Djup dykning
1. Historisk kontext: I äldre versioner av Java användes `Math.random()`, men `java.util.Random` introducerades som ett mer flexibelt och kraftfullt alternativ.
2. Alternativ: Förutom `java.util.Random` kan du också använda `java.util.concurrent.ThreadLocalRandom` i multithreaded miljöer eller `java.security.SecureRandom` för kryptografisk säkra slumpmässiga nummer.
3. Implementeringsdetaljer: `java.util.Random` använder en 48-bitars såkallad "seed" som modifieras med hjälp av en linjär kongruent formel.

## Se även
1. [Java Random Class API](https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/Random.html)
2. [SecureRandom Class in Java](https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/security/SecureRandom.html)
3. [ThreadLocalRandom Class in Java](https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/concurrent/ThreadLocalRandom.html)