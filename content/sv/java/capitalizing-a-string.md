---
title:                "Gör om en sträng till versaler"
html_title:           "Java: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att göra en sträng till versaler innebär att ändra alla tecken i en sträng till stora bokstäver. Programmerare gör detta för att standardisera strängdata och förbättra jämförelse av strängar.

## Hur man gör:
Java tillhandahåller inbyggda metoder för att byta ut små bokstäver till stora. Här är ett exempel:

```Java
public class Main {
    public static void main(String[] args) {
        String str = "hej världen";
        String upperStr = str.toUpperCase();
        System.out.println("Ursprunglig sträng: " + str);
        System.out.println("Sträng i versaler: " + upperStr);
    }
}
```

När du kör ovanstående kod kommer du att få följande utdata:

```
Ursprunglig sträng: hej världen
Sträng i versaler: HEJ VÄRLDEN
```

## Djupdykning
Även om Java: s `toUpperCase()` -metod är den mest använda metoden för att konvertera strängar till versaler, finns det andra alternativ, som `StringUtils.upperCase()`, som tillhandahålls av Apache Commons-biblioteket.

Historiskt sett behövde tidigare versioner av Java implementera denna funktionalitet manuellt genom att iterera över strängen och använda `Character.toUpperCase()` på varje tecken. Dagens versioner har dock inbyggda metoder för att förenkla denna process.

Att göra en sträng till versaler kan tyckas vara en trivial uppgift, men det finns viktiga detaljer bakom implementeringen. Java: s `toUpperCase()` -metod tar hänsyn till språkinställningar och användarinställningarna för att säkerställa korrekt resultat för alla bokstäver, som accent- och specialbokstäver.

## Se även
- [Java String toUpperCase() Metod](https://www.javatpoint.com/java-string-touppercase)
- [Apache Commons StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
- [Java String-dokumentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)