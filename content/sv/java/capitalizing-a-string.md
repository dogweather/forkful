---
title:    "Java: Versaler i en sträng"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng är en vanlig uppgift i programmering som kan vara användbar i olika applikationer. Det kan hjälpa till att förbättra läsbarheten i en text eller identifiera viktiga ord i en sökning.

## Så här gör du

För att kapitalisera en sträng i Java, kan du använda inbyggda metoder som `toUpperCase()` och `toLowerCase()`. Här är ett exempel på hur du kan använda dem:

```Java
String text = "hej världen";
String uppercaseText = text.toUpperCase(); // kommer att returnera "HEJ VÄRLDEN"
String lowercaseText = text.toLowerCase(); // kommer att returnera "hej världen"
```

Du kan också använda `substring()` metoden för att välja vilken del av strängen som du vill kapitalisera:

```Java
String text = "hej världen";
String capitalizedText = text.substring(0, 1).toUpperCase() + text.substring(1); // kommer att returnera "Hej världen"
```

## Djupdykning

I Java, är strängar immutable, vilket innebär att de inte kan ändras efter att de har skapats. När vi kapitaliserar en sträng, skapas en helt ny sträng istället för att den befintliga strängen ändra sig.

Det finns också andra metoder som `replaceFirst()` och `replaceAll()` som kan användas för att kapitalisera enskilda ord i en sträng.

## Se även

* [Java String Documentation](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/String.html)
* [Java String manipulation tutorial](https://www.geeksforgeeks.org/string-class-in-java/)
* [Java substring tutorial](https://www.baeldung.com/java-string-substring)