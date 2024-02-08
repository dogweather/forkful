---
title:                "Ta bort tecken som matchar ett mönster"
aliases:
- sv/java/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:31.165420-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ta bort tecken som matchar ett mönster"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att radera tecken som matchar ett mönster är att plocka bort specifika tecken eller sekvenser av tecken från en sträng. Vi gör det för att rensa data, validera inmatning eller förbereda text för bearbetning.

## Hur man gör:
```java
import java.util.regex.Pattern;

public class PatternMatcher {

    public static void main(String[] args) {
        String input = "H3j! D3t här är en t3xt med siffr0r.";
        String pattern = "\\d"; // \\d matchar alla siffror
        
        String result = deletePattern(input, pattern);
        System.out.println(result); // Skriver ut "Hj! Dt här är en txt med siffror."
    }

    private static String deletePattern(String text, String pattern) {
        return text.replaceAll(pattern, "");
    }
}
```

## Djupdykning
Förr i tiden renoverades text manuellt eller med enklare strängmanipulationsmetoder. Idag använder vi reguljära uttryck (regex), som introducerades i språk som Perl och blev populär i andra språk. I Java hanterar vi dem med `Pattern` och `Matcher` klasserna. 

Alternativ till regex inkluderar traditionell strängmanipulation (såsom att använda `String` metoder `replace()` eller `substring()`) eller externa bibliotek som Apache Commons Lang `StringUtils`. 

Java implementerar regex med en kompileringssteg, vilket innebär att det underliggande mönstret kompileras till en intern representation som sedan används för att matcha text. Prestanda kan variera beroende på mönstrets komplexitet och strängens längd.

## Se även
- [Java Pattern Class](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/util/regex/Pattern.html)
- [Java Matcher Class](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/util/regex/Matcher.html)
- [Regular Expressions in Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
