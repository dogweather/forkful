---
title:    "Java: Att ta bort tecken som matchar ett mönster"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
I Java-programmering kan det vara nödvändigt att ta bort vissa tecken som matchar en viss mönster. Detta kan vara till hjälp när man vill rensa en textsträng från onödiga tecken eller när man vill filtrera en lista av texter.

## Hur man gör det
För att ta bort tecken som matchar ett visst mönster i en textsträng kan man använda metoden `replaceAll()` tillsammans med reguljära uttryck i Java.

```Java
String text = "Hej! Vad är ditt namn?";
String filteredText = text.replaceAll("[^a-zA-Z ]", "");
System.out.println(filteredText); // Output: "Hej Vad är ditt namn"
```

I exemplet ovan använder vi `\[^a-zA-Z ]` som vårt reguljära uttryck, vilket betyder att vi vill behålla alla bokstäver oavsett om de är stora eller små samt mellanslag, men ta bort alla andra tecken.

## Djupdykning
Reguljära uttryck är en viktig del i att ta bort tecken som matchar ett visst mönster. Det finns olika symboler och mönster som kan användas för att matcha specifika tecken eller grupper av tecken. Det är också möjligt att kombinera flera mönster för att få en mer preciserad filtrering.

Det är också viktigt att notera att metoden `replaceAll()` tar emot en sträng som parameter och returnerar en ny filtrerad sträng. Det betyder att den ursprungliga strängen inte ändras.

## Se även
- Oracle: [Regular Expressions in Java](https://docs.oracle.com/javase/tutorial/essential/regex/)
- Regex cheatsheet: [Java Regular Expression](https://www.rexegg.com/regex-java.html)
- RegexPlanet: [Online regex tester and debugger](https://www.regexplanet.com/advanced/java/index.html)