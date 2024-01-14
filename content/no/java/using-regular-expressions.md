---
title:                "Java: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor:
Å forstå og kunne bruke regulære uttrykk i Java er essensielt for å effektivt behandle tekstbasert informasjon. Det er en kraftig verktøy for å søke, erstatte og manipulere tekst på en enkel og elegant måte.

## Hvordan:
Bruken av regulære uttrykk krever kunnskap om bestemte symboler og syntaks. La oss se på et eksempel på å finne og erstatte et mønster i en tekst ved hjelp av regulære uttrykk:

```Java
import java.util.regex.*;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Denne setningen inneholder tallene 1234 og 5678";
        String regex = "[0-9]+";
        String replacement = "X";
        String newText = text.replaceAll(regex, replacement);
        System.out.println(newText);
    }
}

// Output: Denne setningen inneholder tallene X og X
```

Her brukte vi metoden `replaceAll()` fra `String` klassen og sendte inn et regulært uttrykk og en erstatningsstreng. Uttrykket `[0-9]+` betyr "en eller flere tall." Så vi erstattet alle tallene i strengen med bokstaven X.

## Dypdykk:
Regulære uttrykk gir også mulighet for å finne og behandle mønstre basert på mer komplekse kriterier. For eksempel kan vi bruke spesielle symboler som `*`, `?` og `.` for å finne og manipulere tekst basert på mønstre som ikke er nødvendigvis tall. Vi kan også bruke metoder som `matches()` og `find()` for å finne ut om en streng inneholder et spesifikt mønster eller ikke.

## Se også:
- [Java Regular Expressions Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Oracle Documentation on Regular Expressions in Java](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [Java Regex Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)