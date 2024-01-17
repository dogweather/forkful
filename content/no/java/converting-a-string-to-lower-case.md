---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Java: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Konvertering av en streng til små bokstaver er en vanlig operasjon i Java-programmering. Dette gjør det mulig å sammenligne strenger uten å ta hensyn til store og små bokstaver, noe som kan være nyttig i mange tilfeller.

# Hvordan:
```Java
String tekst = "Hei, Dette ER en TEKST";
String konvertertTekst = tekst.toLowerCase();
System.out.println(konvertertTekst);
```
Output:
```
hei, dette er en tekst
```

# Dypdykk:
Konvertering av en streng til små bokstaver har vært en del av Java siden den første utgivelsen i 1996. Metoden `toLowerCase()` blir brukt til å konvertere alle store bokstaver i en streng til tilsvarende små bokstaver. For å konvertere til store bokstaver, kan metoden `toUpperCase()` bli brukt. Alternativt kan man også bruke `equalsIgnoreCase()` metoden til å sammenligne strenger uten å ta hensyn til store og små bokstaver. Konvertering av strenger til små bokstaver kan være nyttig i tilfeller der man trenger å sammenligne eller søke etter tekst på en mer fleksibel måte.

# Se også:
- [Java API Dokumentasjon for `toLowerCase()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Java API Dokumentasjon for `toUpperCase()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [Java API Dokumentasjon for `equalsIgnoreCase()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#equalsIgnoreCase-java.lang.String-)