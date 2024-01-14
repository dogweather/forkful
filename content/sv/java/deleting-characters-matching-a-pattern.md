---
title:                "Java: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett visst mönster är en viktig färdighet inom Java-programmering. Detta gör det möjligt för dig att filtrera eller manipulera data på ett effektivt sätt och förbättra prestandan för ditt program.

## Hur du gör det

För att ta bort tecken som matchar ett visst mönster i Java, kan du använda metoden `replaceAll()` från klassen `java.lang.String`. Denna metod tar emot två parametrar - det mönster du vill matcha och den byte du vill ersätta den matchade biten med. Här är ett exempel:

```java
String input = "Rensa bort alla specialtecken i den här strängen!";
String output = input.replaceAll("[^a-zA-Z0-9 ]", "");
System.out.println(output);
```

I det här exemplet använder vi en regex som matchar alla specialtecken (inklusive mellanslag) och ersätter dem med en tom sträng. Det ger oss en ren version av vår ursprungliga sträng. Koden ovan kommer att producera följande output:

```
Rensa bort alla specialtecken i den har strangen
```

Observera att vi använder `^` för att ange ett negativt tecken inom hakparenteser, vilket betyder att vi vill matcha allt utom det som finns innanför dem. Om du vill ta bort specifika tecken, till exempel endast specialtecken, kan du använda ett uttryck som `"[!-/:-@\\[-`-`{-~]"`i stället.

## Djupdykning

För att förstå detta bättre, låt oss titta på hur regex fungerar. En regex är ett uttryck för att matcha eller hitta ett visst mönster i en sträng. Det är ett mycket kraftfullt verktyg inom programmering, men det kan också vara förvirrande att förstå för nybörjare.

I exemplet ovan använder vi en vanlig regex syntax `[...]` för att definiera ett set av tecken som vi vill matcha. Inuti hakparenteserna kan vi använda teckenklasser (t.ex. `a-z` eller `A-Z`) eller enskilda tecken. Om vi vill matcha tecken mellan `a` och `e` skulle vi använda `"[a-e]"`.

Vi kan också använda modifierare i vår regex för att ändra dess beteende. Till exempel betyder `i` att matchningen är fallöverskridande. Om vi inte skulle ha angett det, skulle endast tecken som matchar exakt vårt uttryck tas bort.

Det finns en hel del olika speciala tecken och mönster som du kan använda för att skapa regex och överväga. Det kan vara en bra idé att ta en titt på Java-dokumentationen eller annan resurs för att bekanta dig med dessa för att bli bekväm med regex.

## Se också

- [Java Regex-tutorial](https://www.tutorialspoint.com/java/java_regular_expressions.htm)
- [Java String-metoder](https://www.w3schools.com/java/java_ref_string.asp)
- [Java Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)

Tack för att du läste! Vi hoppas att denna artikel har hjälpt dig att förstå hur du kan ta bort tecken som matchar ett mönster i Java. Lycka till med din fortsatta programmeringsresa!