---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:18.022081-07:00
description: "Debug output afdrukken is het gooien van kleine broodkruimels van informatie\
  \ in de console om fouten op te sporen. Het is snel, vies, en effectief om te\u2026"
lastmod: '2024-03-13T22:44:50.684663-06:00'
model: gpt-4-0125-preview
summary: "Debug output afdrukken is het gooien van kleine broodkruimels van informatie\
  \ in de console om fouten op te sporen. Het is snel, vies, en effectief om te\u2026"
title: Debug-output afdrukken
---

{{< edit_this_page >}}

## Wat & Waarom?
Debug output afdrukken is het gooien van kleine broodkruimels van informatie in de console om fouten op te sporen. Het is snel, vies, en effectief om te begrijpen wat er binnen je code gebeurt wanneer deze op hol slaat.

## Hoe te:
Laten we wat code op het scherm krijgen:

```java
public class DebugExample {
    public static void main(String[] args) {
        int som = 0;
        for (int i = 1; i <= 10; i++) {
            som += i;
            System.out.println("Toegevoegd " + i + ", som nu: " + som);
        }
    }
}
```

Dit fragment somt getallen op van 1 tot 10 en drukt de voortgang af:

```
Toegevoegd 1, som nu: 1
Toegevoegd 2, som nu: 3
...
Toegevoegd 10, som nu: 55
```

## Diepgaand Onderzoek
Voordat IDE's slim werden, was printf-stijl debuggen de standaard. Zelfs nu, te midden van fancy breakpoints, is soms een goed geplaatste `System.out.println()` alles wat je nodig hebt om de planeten uit te lijnen.

Alternatieven? Loggingframeworks zoals Log4J of SLF4J geven je controle over debuginformatie, scheiden het van systeemoutput en laten je de uitvoerigheid aanpassen.

Wat implementatie betreft, onthoud dat `System.out` een `PrintStream` object is, dat standaard naar stdout uitvoert. Het kan worden vervangen om output om te leiden, waardoor testen of loggen minder opdringerig wordt.

## Zie Ook
- [Oracle’s Tutorial over I/O Streams](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [Best Practices voor Loggen in Java](https://www.baeldung.com/java-logging-intro)
- [SLF4J Documentatie](http://www.slf4j.org/docs.html)
