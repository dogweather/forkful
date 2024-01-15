---
title:                "Att hitta längden på en sträng"
html_title:           "Java: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att veta längden på en sträng är en vanlig uppgift i Java-programmering. Det kan hjälpa till att bestämma hur mycket minne som behövs för att lagra strängen och göra det möjligt att manipulera den på rätt sätt.

## Hur man gör det

För att hitta längden på en sträng i Java, använd metoden `length ()` från `String` -klassen. Här är ett enkelt exempel:

```Java
String myString = "Hej, jag heter Johan";
int length = myString.length();
System.out.println(length);
```

Output:
```
19
```

## Djupdykning

Metoden `length ()` returnerar antalet tecken i en sträng. Detta innebär att alla tecken räknas, även mellanslag och specialtecken. Om du vill hitta längden på en sträng utan mellanslag, kan du använda metoden `replace ()` för att ta bort dem först.

Det finns också andra sätt att hitta längden på en sträng, som att använda en loop för att räkna antalet tecken eller använda `StringTokenizer` -klassen. Men att använda `length ()` -metoden är den enklaste och mest effektiva metoden.

## Se även

- [Java String Class](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [String length() method](https://www.geeksforgeeks.org/java-string-length-method-example/)
- [Efficiency of String length() method](https://www.journaldev.com/18048/string-length-java)