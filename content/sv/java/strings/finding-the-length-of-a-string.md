---
date: 2024-01-20 17:47:36.832486-07:00
description: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken i den. Programmerare g\xF6r det f\xF6r att validera indata, begr\xE4nsa\
  \ inneh\xE5ll, loopa korrekt och\u2026"
lastmod: '2024-03-13T22:44:37.778671-06:00'
model: gpt-4-1106-preview
summary: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken i den."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

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
