---
title:                "Een string met hoofdletters maken"
aliases: - /nl/java/capitalizing-a-string.md
date:                  2024-01-28T21:56:16.578357-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een string met hoofdletters maken betekent de eerste letter in hoofdletters zetten en de rest van de letters in kleine letters. Programmeurs gebruiken dit om tekstinvoer te normaliseren, zoals gebruikersinvoer of namen, om consistentie in een dataset te waarborgen.

## Hoe:

In Java is er geen ingebouwde methode om een string volledig met hoofdletters te maken (eerste letter in hoofdletter, rest in kleine letters), maar hier is een snelle functie om precies dat te doen:

```java
public class StringCapitalizer {
    public static void main(String[] args) {
        String input = "java is leuk!"; // voorbeeldstring
        String output = capitalizeString(input);
        System.out.println(output); // Java is leuk!
    }

    public static String capitalizeString(String str) {
        if(str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1).toLowerCase();
    }
}
```

## Diepere Duik

Voor Java 8 was de bovenstaande methode een gebruikelijke manier om een string met hoofdletters te maken. Sinds de introductie van streams in Java 8, kunnen we ook strings met meer flexibiliteit manipuleren.

Een alternatieve manier om met streams hoofdletters te gebruiken:

```java
import java.util.stream.*;

public class StringCapitalizer {
    public static void main(String[] args) {
        String input = "java is cool!";
        String output = Arrays.stream(input.split("\\s"))
                              .map(word -> word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase())
                              .collect(Collectors.joining(" "));
        System.out.println(output); // Java Is Cool!
    }
}
```

Dit splitst de string in woorden, zet elk woord in hoofdletters en voegt ze weer samen. Let op het verschil: elk woord wordt met een hoofdletter geschreven, niet alleen de eerste.

Strings zijn onveranderbaar in Java, wat betekent dat ze, eenmaal aangemaakt, niet kunnen veranderen. Methoden die lijken te wijzigen strings, zoals `toUpperCase` of `toLowerCase`, maken eigenlijk nieuwe strings aan met de aangebrachte wijzigingen.

Wat betreft prestaties wordt StringBuilder vaak gebruikt voor stringmanipulatie, omdat deze veranderlijk is. Het vermijdt de kosten van het aanmaken van meerdere string-objecten. Echter, voor eenvoudige hoofdlettergebruik is de prestatiewinst geen grote zaak, daarom is een `StringBuilder`-voorbeeld weggelaten.

## Zie Ook

- [Java String API Documentatie](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Collector Documentatie](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html)
- [StringJoiner Documentatie](https://docs.oracle.com/javase/8/docs/api/java/util/StringJoiner.html)
