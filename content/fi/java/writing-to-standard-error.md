---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
"Heitä ja miksi?"

Java-sovelluksissa virheenkirjoitus (`System.err`) on tapa ohjata virheviestit oikeaan kanavaan. Ohjelmoijat käyttävät sitä erottaakseen tavallisen tulosteen odottamattomista virheistä ja debug-viesteistä.

## How to:
"Kuinka tehdä:"

Koodissa käytetään `System.err.println()` virheilmoitusten näyttämiseen:

```java
public class ErrorLogging {

    public static void main(String[] args) {
        System.out.println("Tavallinen viesti"); // Normaali tulostus
        System.err.println("Virheilmoitus");     // Virhetulostus
    }
}
```

Tulostuu konsoliin näin:

```
Tavallinen viesti
Virheilmoitus
```

## Deep Dive
"Sukellus syvyyksiin"

Historiallisesti `System.err` on ollut osa Javan standardikirjastoa versiosta 1.0 lähtien. Vaihtoehtoina voi käyttää lokituskehyksiä (esim. SLF4J tai Log4j), jotka tarjoavat lisäominaisuuksia kuten lokitiedostot ja eri tasoilla lokittamisen. `System.err` toteutetaan `PrintStream`-objektina, jota voi vaihtaa omiin tarkoituksiin `System.setErr()`-metodilla.

## See Also
"Katso myös"

- Oracle Java dokumentaatio: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err
- SLF4J-kehys: http://www.slf4j.org/
- Log4j: https://logging.apache.org/log4j/2.x/