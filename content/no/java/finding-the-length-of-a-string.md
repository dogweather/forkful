---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng innebærer å telle antall karakterer i den. Programmerere gjør dette for å kontrollere datainngang, løkke gjennom strenger, skape substrings og mer.

## Hvordan gjøre det:

Her er hvordan du finner lengden på en streng i Java:

```Java
String tekst = "Hallo, Norge!";
int lengde = tekst.length();
System.out.println(lengde);
```

Utskriften vil være:

```Java
14
```

## Dypdykk:

Å finne strengens lengde har vært en vanlig praksis siden programmeringsspråkets gryende start. I Java, gir `length()`-metoden i `String`-klassen oss direkte lengde av strengen.

Alternativt kan vi også bruke `toCharArray()` for å konvertere strengen til en array av tegn og deretter bruke `length`-feltet.

```Java
String tekst = "Hei alle sammen";
int lengde = tekst.toCharArray().length;
System.out.println(lengde);
```

Å finne lengden på en streng i Java er rask og effektivt takket være direkte indeksering og bruk av tegnarrays.

## Se Også:

For mer dyptgående kunnskap om strenger i Java, se følgende ressurser:

1. [Java String dokumentasjon](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
2. [Java String lengde metode](https://www.javatpoint.com/java-string-length)
3. [Java-strenger (Oracle Guides)](https://docs.oracle.com/javase/tutorial/java/data/strings.html)