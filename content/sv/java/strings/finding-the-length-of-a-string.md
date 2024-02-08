---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:47:36.832486-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Att hitta längden på en sträng innebär att räkna antalet tecken i den. Programmerare gör det för att validera indata, begränsa innehåll, loopa korrekt och mycket annat.

## How to: (Hur man gör:)
Java gör det enkelt. Använd `length()` metod på en sträng objekt och du har svaret. Så här:

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String hello = "Hej Sverige!";
        int length = hello.length();
        System.out.println("Längden av strängen är: " + length);
    }
}
```
Kör koden. Output blir:
```
Längden av strängen är: 12
```

## Deep Dive (Djupdykning)
De goda gamla dagarna använde vi `length` som en array egenskap. Men i Java, `String` är ett objekt, inte en array. Därför använder vi en metod.

Alternativ? Absolut. I vissa språk, tänk JavaScript, använder man fortfarande en egenskap – `.length`. I Java kan du fortfarande stöta på `StringBuilder` eller `StringBuffer` som också har en `length()` metod.

Implementering? `length()` returnerar ett `int` som representerar antalet `char` i `String`-objektet. Den räknar Unicode-tecken, inklusive specialtecken och spaces.

## See Also (Se även)
- [Oracle Java Documentation on Strings](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Tutorial on Java Strings from W3Schools](https://www.w3schools.com/java/java_strings.asp)
- [Java String API Specs](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length())
