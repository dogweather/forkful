---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför? 

Att konvertera en sträng till gemener innebär att förändra alla stora bokstäver till små bokstäver inom en given sträng. Programmerare gör detta för att standardisera dataingångar och underlättar jämforanden.

## Hur man gör:

Här är hur du konverterar en sträng till gemener i Java:

```Java 
String str = "Hej Världen!"; // din sträng
String lowerCaseStr = str.toLowerCase(); // konvertera till gemener
System.out.println(lowerCaseStr); // printa resultatet
```
När du kör ovanstående kod kommer utfallet att vara:

```Java
"hej världen!"
```
## Djupdykning:

Strängar till gemener används över hela världen och detta har pågått sedan ASCIIs tidiga dagar. Alternativen till denna metod i Java inkluderar att använda ASCII-värden direkt, eller använda tredje parts bibliotek.

När det gäller informan om hur denna teknik implementeras i Java: metoden toLowerCase() i strängklassen använder internationella standarder fastställda av Unicode för att bestämma vilken liten bokstav som motsvarar varje stor bokstav.

## Se även:

För mer information om strängar och teckenhantering i Java, ta en titt på följande länkar:
- [Oracle Official Java Documentation](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Java Tutorial Point](https://www.javatpoint.com/java-string-tolowercase)