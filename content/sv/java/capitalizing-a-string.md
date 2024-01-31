---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att förvandla alla bokstäver i strängen till versaler. Programmerare gör detta för att standardisera textdata, betona viktiga ord eller för att uppfylla tekniska krav.

## Hur gör man:
```java
public class StringCapitalizer {
    
    public static void main(String[] args) {
        String text = "hej världen";
        String capitalizedText = text.toUpperCase();
        System.out.println(capitalizedText); // HEJ VÄRLDEN
    }
}
```
Kör programmet. Utmatningen blir `HEJ VÄRLDEN`.

## Djupdykning:
Kapitalisering av strängar har länge varit en grundläggande operation i programmering. I tidiga dagar av datoranvändning, speciellt i system som bara stödde versaler, var detta absolut nödvändigt.

Det finns alternativ till metoden `toUpperCase()`, till exempel `toLowerCase()` som gör motsatsen. Man kan också använda Apache Commons `StringUtils.capitalize()` för att endast göra första bokstaven stor.

När du kapitaliserar en sträng i Java, används under huven Unicode och Character-klassen för korrekt konvertering, även för bokstäver utanför ASCII-omfånget.

## Se även:
- [Oracle Java docs on toUpperCase](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#toUpperCase())
- [Wikipedia om Unicode](https://sv.wikipedia.org/wiki/Unicode)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
