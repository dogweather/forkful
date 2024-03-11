---
date: 2024-01-20 17:52:55.407095-07:00
description: "\xC5 skrive ut debug informasjon betyr \xE5 printe ut data for \xE5\
  \ forst\xE5 hva koden din gj\xF8r eller finne feil. Programmerere gj\xF8r dette\
  \ for \xE5 se programflyten\u2026"
lastmod: '2024-03-11T00:14:14.210294-06:00'
model: gpt-4-1106-preview
summary: "\xC5 skrive ut debug informasjon betyr \xE5 printe ut data for \xE5 forst\xE5\
  \ hva koden din gj\xF8r eller finne feil. Programmerere gj\xF8r dette for \xE5 se\
  \ programflyten\u2026"
title: "Skrive ut feils\xF8kingsdata"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive ut debug informasjon betyr å printe ut data for å forstå hva koden din gjør eller finne feil. Programmerere gjør dette for å se programflyten eller verdiene som brukes under kjøring.

## Slik gjør du:
Bruk `System.out.println()` for enkel output til konsollen.

```java
public class DebugDemo {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 1; i <= 5; i++) {
            System.out.println("Legger til: " + i);
            sum += i;
            System.out.println("Summen er nå: " + sum);
        }
    }
}
```

Eksempel på utskrift:
```
Legger til: 1
Summen er nå: 1
Legger til: 2
Summen er nå: 3
Legger til: 3
Summen er nå: 6
Legger til: 4
Summen er nå: 10
Legger til: 5
Summen er nå: 15
```

## Dypdykk
Debug-utskrift er ikke ny. Det stammer fra de tidlige dagene av programmering da utviklere trengte måter å forstå og kontrollere programkjøringen på. Alternativer til `System.out.println()` inkluderer loggerrammer som `Log4j` eller `SLF4J` som tilbyr komplekse loggnivåer og formattering. Disse rammene hjelper til med å strukturere output bedre og er enklere å slå av i produksjon.

Loggerrammene gjør det også mer håndterbart å eksportere loggene til eksterne systemer for analyse, noe som er vanskelig med enkel konsollutskrift. I Java kan implementering av logging også gjøres ved bruk av `java.util.logging`, som er innebygd i standardbiblioteket.

## Se også
- [Logger (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/logging/Logger.html)
- [SLF4J Project Page](http://www.slf4j.org/)
- [Apache Log4j 2](https://logging.apache.org/log4j/2.x/)
