---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
simple_title:         "Använda reguljära uttryck"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönster för att matcha textsträngar. Programmerare använder detta för textbearbetning, dataextraktion och validering.

## Hur man gör:
```Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
  public static void main(String[] args) {
    String text = "Hej! Mitt telefonnummer är 070-123 45 67.";
    String patternString = "\\d{3}-\\d{3} \\d{2} \\d{2}";

    Pattern pattern = Pattern.compile(patternString);
    Matcher matcher = pattern.matcher(text);

    if (matcher.find()) {
      System.out.println("Hittade nummer: " + matcher.group());
    }
  }
}
```
Utskrift:
```
Hittade nummer: 070-123 45 67
```

## Fördjupning
Reguljära uttryck härstammar från teoretisk datavetenskap och automatteori. Alternativ till Java's inbyggda klasser inkluderar bibliotek som Apache Commons Lang. Implementationen använder vanligtvis tillståndsmaskiner för att tolka och matcha mönstren.

## Se även
- Java Dokumentation på Pattern Class: https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html
- Oracle Tutorial på reguljära uttryck: https://docs.oracle.com/javase/tutorial/essential/regex/
- Apache Commons Lang: https://commons.apache.org/proper/commons-lang/
