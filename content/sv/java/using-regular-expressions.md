---
title:                "Att använda reguljära uttryck"
html_title:           "Java: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Regular expressions, även kallat regex, är ett kraftfullt verktyg som används inom programmering för att söka och manipulera textsträngar. Det är särskilt användbart när man behöver söka efter specifika mönster eller utföra komplexa uppgifter som annars skulle kräva mycket kod.

## Så här använder du regex i Java
För att använda regular expressions i Java behöver vi importera java.util.regex-biblioteket. Sedan kan vi använda dess olika metoder som tillhandahåller möjligheter för att söka och manipulera textsträngar.

Första steget är att skapa ett Regular Expression-objekt med hjälp av metoden `compile()` och ange det mönster som vi vill söka efter som en parameter. Sedan använder vi metoden `matcher()` på vårt regex-objekt med den textsträng vi vill söka igenom som parameter. Till exempel:

```
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
  public static void main(String[] args) {
    // Skapar ett regex-objekt med mönstret "abc"
    Pattern pattern = Pattern.compile("abc");

    // Skapar en matcher med vår textsträng "abcdefg"
    Matcher matcher = pattern.matcher("abcdefg");

    // Använder find() för att söka efter mönstret i textsträngen
    // Returnerar true om mönstret hittas, annars false
    System.out.println(matcher.find()); // kommer att skriva ut "true"
  }
}
```

I exemplet ovan kommer `matcher.find()` att returnera `true` eftersom "abc" förekommer i textsträngen "abcdefg". Om du vill söka efter flera förekomster av ett mönster, använd `find()` inuti en loop. Det finns även andra metoder som `matches()` och `replaceAll()` som också är användbara beroende på vad du behöver göra med din textsträng.

## Fördjupning i användningen av regex
Att behärska regular expressions kan underlätta många uppgifter inom programmering. Det finns en mängd olika symboler och uttryck som kan användas för att skapa mönster och söka efter dem i textsträngar. Till exempel:

- `.` används för att matcha alla tecken.
- `*` används för att matcha noll eller flera förekomster av ett tecken.
- `[ ]` kan användas för att specificera en uppsättning tecken som ska matchas.
- `^` och `$` används för att matcha början och slutet av en textsträng.
- `()` används för att gruppera delar av ett mönster.

Det finns många andra uttryck och mönster att lära sig, men det är en bra start att använda de mest grundläggande. Sedan kan du testa och experimentera för att skapa mer avancerade regex-uttryck.

## Se även
- [Java Regular Expressions](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Java Regex Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)