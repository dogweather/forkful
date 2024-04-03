---
date: 2024-01-26 03:40:56.009629-07:00
description: "Hvordan: La oss rive disse irriterende anf\xF8rselstegnene ut av teksten\
  \ v\xE5r. Vi vil bruke `replace()`-metoden for de raske fiksene og regex for de\
  \ t\xF8ffe\u2026"
lastmod: '2024-03-13T22:44:40.654285-06:00'
model: gpt-4-0125-preview
summary: "La oss rive disse irriterende anf\xF8rselstegnene ut av teksten v\xE5r."
title: "Fjerne anf\xF8rselstegn fra en streng"
weight: 9
---

## Hvordan:
La oss rive disse irriterende anførselstegnene ut av teksten vår. Vi vil bruke `replace()`-metoden for de raske fiksene og regex for de tøffe nøttene å knekke.

```java
public class QuoteRemover {
    public static void main(String[] args) {
        String stringWithQuotes = "\"Hello, 'World'!\"";
        String withoutQuotes = stringWithQuotes.replace("\"", "").replace("'", "");
        System.out.println(withoutQuotes); // Hello, World!

        // Nå med regex for mønsterentusiastene
        String stringWithMixedQuotes = "\"Java\" and 'Programming'";
        String cleanString = stringWithMixedQuotes.replaceAll("[\"']", "");
        System.out.println(cleanString); // Java and Programming
    }
}
```

## Dypdykk
Tilbake i tiden var ikke anførselstegn i strenger for mye å bry seg om—systemene var enklere, og dataene var ikke så rotete. Med fremveksten av komplekse dataformater (JSON, XML) og behovet for datautveksling, ble håndtering av anførselstegn nøkkelen. Når det gjelder alternativer, klart, du kunne skrive en parser, loope gjennom hver karakter og bygge en ny streng (kan være gøy på en regnværsdag). Det finnes også tredjepartsbiblioteker som kan håndtere dette med mer sofistikasjon, som tilbyr alternativer for å unnslippe tegn i stedet for å fjerne dem, eller å håndtere forskjellige typer anførselstegn i henhold til lokalområde. Implementeringsmessig, husk at å fjerne anførselstegn uten kontekst kan endre meningen eller strukturen til data—vurder alltid "hvorfor" før "hvordan".

## Se Også
- For et dypdykk inn i regex, sjekk ut de offisielle Java-dokumentene: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- Trenger du å unnslippe anførselstegn i stedet for å fjerne dem? Stack Overflow har ryggen din: https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- JSON-behandling i Java? Du vil sannsynligvis ofte møte anførselstegn. Her er et utgangspunkt: https://www.oracle.com/technical-resources/articles/java/json.html
