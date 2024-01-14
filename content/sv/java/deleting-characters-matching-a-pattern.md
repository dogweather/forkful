---
title:                "Java: The original article in Swedish for Deleting characters matching a pattern is Att ta bort tecken som matchar ett mönster."
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Varför

I många Java-programmeringsprojekt stöter man ofta på situationer där man behöver manipulera strängar på olika sätt. En vanlig åtgärd är att ta bort vissa karaktärer som matchar ett visst mönster. Detta kan vara till nytta när man exempelvis vill rensa bort specialtecken eller whitespace från en sträng. I denna bloggpost kommer jag att gå igenom hur man kan utföra denna åtgärd på ett enkelt sätt med hjälp av Java.

##Hur man gör det

För att ta bort karaktärer som matchar ett visst mönster från en sträng i Java, kan man använda metoden `replaceAll()` från klassen `java.lang.String`. Denna metod tar två argument - en sträng som representerar mönstret man vill matcha, och en sträng som representerar ersättningen för de matchande karaktärerna.

```Java
String text = "#Programming is fun and rewarding!";
text = text.replaceAll("[^a-zA-Z0-9 ]", "");
System.out.println(text);
```

I det här exemplet använder vi mönstret `[^a-zA-Z0-9 ]` för att matcha alla icke-bokstaver, icke-siffror och icke-whitespace karaktärer i strängen `text`. Sedan ersätter vi de matchande karaktärerna med en tom sträng, vilket i praktiken tar bort dem från strängen. Resultatet blir "Programming is fun and rewarding!".

Man kan även använda `replaceAll()` för att ta bort en viss karaktär eller en grupp av karaktärer från en sträng. Till exempel kan man ta bort alla mellanslag från en sträng genom att använda `text.replaceAll(" ", "")`.

##Djupdykning

För att förstå lite mer om hur `replaceAll()` fungerar, kan det vara bra att känna till att metoden egentligen använder sig av reguljära uttryck för att matcha karaktärer. Reguljära uttryck är en kraftfull sök- och ersättningsmekanism som finns tillgänglig i många programmeringsspråk. De kan användas för att matcha komplexa mönster i en sträng, vilket är anledningen till att `replaceAll()` är så flexibel.

##Se även

- [Java Doc: String replaceAll()](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
- [Java Regular Expressions Tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/)