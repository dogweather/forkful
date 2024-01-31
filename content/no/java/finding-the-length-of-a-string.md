---
title:                "Finn lengden på en streng"
date:                  2024-01-20T17:47:34.598880-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finne lengden på en streng betyr å telle antall tegn den inneholder. Programmerere trenger dette for validering, looping og manipulering av tekstdata.

## How to:
I Java bruker du `.length()`-metoden for å få lengden på en streng. Her er et enkelt eksempel:

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String greeting = "Hei, Norge!";
        int length = greeting.length();
        System.out.println("Lengden av strengen er: " + length);
    }
}
```

Kjører du dette, vil utskriften være:
```
Lengden av strengen er: 11
```

## Deep Dive
Metoden `.length()` ble introdusert i Java 1.0, så den er like gammel som språket selv. Alternativer inkluderer å bruke en `for`-løkke for å telle tegn manuelt, men det er mindre effektivt. `.length()` er rask fordi strenglengder i Java lagres som en del av strengobjektene, så det er bare en simpel henting av en verdi, ikke en beregning.

## See Also
- [Java String documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Oracle Java tutorials – Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Java Performance - The Definitive Guide av Scott Oaks](http://shop.oreilly.com/product/0636920028499.do) for diskusjoner rundt ytelse og strings.
