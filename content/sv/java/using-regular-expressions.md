---
title:                "Använda reguljära uttryck"
html_title:           "Gleam: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck, eller regex, är starka verktyg för att matcha, hitta och manipulera strängar i Java. Programmers använder dem för att effektivisera och förenkla textbearbetning och dataextraktion.

## Hur man gör:
Att arbeta med regex i Java är relativt enkelt. Här är några exempel:

```Java
import java.util.regex.*;

public class Main {
    public static void main(String[] args) {
        // Skapa ett mönster att matcha emot
        Pattern pattern = Pattern.compile("hej");
        // Skapa en matchare med mönstret
        Matcher matcher = pattern.matcher("hej där");

        // Kontrollera om mönstret matchar
        boolean matchFound = matcher.find();
        System.out.println(matchFound); // Skriver ut 'true' eftersom "hej" finns i "hej där"
    }
}
```

## Djupdykning
Reguljära uttryck härstammar från teoretisk datavetenskap, men har blivit väsentliga verktyg för praktisk kodning. Alternativ till regex i Java inkluderar String-klassens metoder som `contains()`, `startsWith()`, osv. Men dessa metoder tillåter inte komplexa matchningskrav som regex gör. Regex i Java implementeras via java.util.regex-biblioteket som innehåller `Pattern`- och `Matcher`-klasserna.

## Se också
Följande resurser erbjuder mer detaljerade insikter om regex och dess användning i Java:
1. [Java Docs: Class Pattern](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/regex/Pattern.html)
2. [Java Docs: Class Matcher](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/regex/Matcher.html)
3. [RegExr: Learn, Build, & Test RegEx](https://regexr.com/)