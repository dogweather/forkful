---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke, oder Regex, ermöglichen das Suchen und Manipulieren von Text mittels Mustern. Sie sind extrem nützlich zur Validierung, Formatierung und Extraktion von Daten aus Zeichenketten.

## How to:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexBeispiel {
    public static void main(String[] args) {
        String text = "Hallo, ich habe 15 Äpfel und 12 Birnen.";
        String regex = "\\b\\d+\\b";
        
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(text);

        while(matcher.find()) {
            System.out.println(matcher.group());
        }
    }
}
```
**Ausgabe:**
```
15
12
```

## Deep Dive

Reguläre Ausdrücke sind seit den 1950er Jahren Teil der Informatik, erdacht von Stephen Cole Kleene. Java hat sie durch das `java.util.regex`-Paket im JDK 1.4 eingeführt. Alternativen zu Regex sind Parser für strukturierte Daten, automatische Texterkennung über KI oder spezifische String-Operationen. In Java ist die Regex-Verarbeitung relativ effizient, kann aber bei komplexen Mustern langsam sein.

## See Also

- [Die offizielle Java-Dokumentation zu Pattern](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Oracle Java Tutorial zu regulären Ausdrücken](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [RegExr: Lernwerkzeug und Community für Regex](https://regexr.com/)
