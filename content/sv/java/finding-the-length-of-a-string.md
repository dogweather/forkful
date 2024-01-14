---
title:    "Java: Att hitta längden på en sträng"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin undrat hur många tecken som finns i en viss text? Kanske håller du på att skriva en programvara som behöver beräkna längden på en sträng? Oavsett anledningen finns det flera sätt att hitta längden på en sträng i Java.

## Hur man gör

För att hitta längden på en sträng i Java kan du använda metoden `length()` som finns inbyggd i klassen `String`. Här är ett enkelt exempel som visar hur man använder den:

```java
// Skapa en sträng
String text = "Hej, världen!";

// Hitta längden på strängen och spara den i en variabel
int längd = text.length();

// Skriv ut längden
System.out.println("Längden på strängen är: " + längd);

// Output: Längden på strängen är: 14
```

Som du kan se i exemplet ovan, använder vi metoden `length()` tillsammans med namnet på vår sträng och sparar resultatet i en variabel. Därefter kan vi skriva ut längden genom att använda variabeln tillsammans med en textsträng.

## Djupdykning

Det finns flera saker att tänka på när det kommer till att hitta längden på en sträng i Java. En viktig sak att komma ihåg är att längden inkluderar alla tecken, även mellanslag och specialtecken. Detta kan påverka hur din programvara bearbetar och presenterar strängar.

En annan intressant sak att notera är att metoden `length()` egentligen är en egenskap för objektet `String`. Det betyder att du kan använda den utan att behöva skapa en ny instans av klassen `String`.

## Se även

- Dokumentation för `String`-klassen i Java: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Java String Tutorial på W3Schools: https://www.w3schools.com/java/java_string.asp
- En uttömmande guide om strängar i Java: https://www.baeldung.com/java-strings