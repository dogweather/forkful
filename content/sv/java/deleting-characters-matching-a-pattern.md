---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att ta bort tecken som matchar ett mönster är en vanlig operation inom programmering, där vissa specifika tecken i en sträng identifieras och tas bort baserat på ett visst mönster, kallat ett "regular expression" eller "regex". Detta är användbart för att rensa upp data, extrahera information, och att manipulera strängar på sofistikerade sätt.

## Så här gör du:
Här är ett exempel på hur man tar bort tecken som matchar en regex-mönster i Java:

```Java
import java.util.regex.*;

public class Main {
    public static void main(String[] args) {
        String input = "Hej1 Hej2 Hej3";
        String pattern = "\\d"; // regex pattern to match digits

        Pattern p = Pattern.compile(pattern);
        Matcher m = p.matcher(input);

        String cleaned = m.replaceAll(""); 
        // This will remove all numbers from the string

        System.out.println(cleaned);  // Output: Hej Hej Hej
    }
}
```
## Djupdykning
Denna metod att ta bort tecken baserat på kleenska stjärnor visades först i reguljära uttryck, föddes ur formell språkteori och automata inom datavetenskap under 1950-talet. Alternativ till detta inkluderar att använda 'split' metoden på en sträng, men det ger inte samma grad av kontroll och flexibilitet. 

Innan du använder detta i din kod, var medveten om att kompliceras regex-mönster kan vara långsamma att kompilera och att exekvera. Det är ibland bättre att skriva en mer explicit loop istället för en komplicerad regex, beroende på dina prestandakrav. 

## Se även
För mer detaljer, se Java dokumentationen för `Pattern` (https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/regex/Pattern.html) och `Matcher` (https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/regex/Matcher.html) klasser och Oracle's guide till reguljära uttryck (https://docs.oracle.com/javase/tutorial/essential/regex/).