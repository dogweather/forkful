---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Le espressioni regolari (regex) permettono di cercare e manipolare stringhe usando pattern definiti. I programmatori le usano per validare input, estrarre dati e semplificare complessi task di testo.

## How to:
```java
import java.util.regex.*;

public class RegexExample {
    public static void main(String[] args) {
        // Pattern per trovare numeri in una stringa
        Pattern p = Pattern.compile("\\d+");
        // Matcher per cercare il pattern nell'input
        Matcher m = p.matcher("Sono nati 33 gattini e 44 cuccioli.");
        
        // Trova e stampa tutti i match
        while (m.find()) {
            System.out.println(m.group());
        }
    }
}
```
**Output:**
```
33
44
```

## Deep Dive
Le regex in Java risalgono a Java 1.4 e si appoggiano alla libreria `java.util.regex`. Rispetto ad altri linguaggi, Java non offre sintassi regex integrata, ma richiede la creazione di oggetti `Pattern` e `Matcher`. Come alternativa, Java possiede metodi come `String.split` e `String.matches` per piccoli task. L'implementazione di regex in Java è basata sulla libreria open-source Apache Jakarta ORO, derivata dall'originale Perl 5's syntax.

## See Also
- [Oracle Java docs on regex](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html)
- [Tutorial di Oracle sulle regex Java](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regular-Expressions.info per approfondimenti](https://www.regular-expressions.info/java.html)
