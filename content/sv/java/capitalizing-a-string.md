---
title:                "Att Göra En Sträng Med Stor Bokstav"
html_title:           "Java: Att Göra En Sträng Med Stor Bokstav"
simple_title:         "Att Göra En Sträng Med Stor Bokstav"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att göra en sträng stor bör man göra när man vill betona vissa delar av den, till exempel i en rubrik eller en titel. Det är ett enkelt sätt att göra texten mer lättläst och tydlig.

## Hur man gör

Det finns ett enkelt sätt att göra en sträng stor i Java genom att använda metoden `toUpperCase()`. Se nedan för kodexempel:

```Java
String minSträng = "hej världen";
String storSträng = minSträng.toUpperCase();
System.out.println(storSträng);
```

Output: HEJ VÄRLDEN

## Fördjupning

Det finns flera saker att tänka på när man gör en sträng stor i Java:

- Metoden `toUpperCase()` omvandlar alla bokstäver i en sträng till versaler 
- Om du vill konvertera bara första bokstaven i varje ord, använd metoden`capitalize()`
- Om du behöver ändra storleken på en specifik del av en sträng, kan du använda `substring()` metoden tillsammans med `toUpperCase()`

Se nedan för ett exempel på användning av `substring()`:

```Java
String minSträng = "hej världen";
String förstaDel = minSträng.substring(0, 3).toUpperCase();
String andraDel = minSträng.substring(3);
String storSträng = förstaDel + andraDel;
System.out.println(storSträng);
```

Output: HEJ världen

## Se även

- [Java String Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [String manipulation in Java](https://www.geeksforgeeks.org/string-class-java/)
- [How to make first letter of a string uppercase in Java](https://www.baeldung.com/java-uppercase-first-letter)