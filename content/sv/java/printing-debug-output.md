---
title:                "Utskrift av felsökningsutdata"
html_title:           "Java: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut debuggade utdata är ett sätt för programmerare att se vad som händer i sin kod medan den körs. Detta gör det möjligt att identifiera felaktigheter och spåra koden för att lösa problem.

## Så här:
För att skriva ut debuggade utdata i Java, används metoden `System.out.println()`. Detta låter dig skriva ut en sträng av text eller värdet på en variabel. Till exempel:

```java
String namn = "Anna";
int ålder = 25;
System.out.println("Hej, mitt namn är " + namn + " och jag är " + ålder + " år gammal.");
```

Detta skulle producera utdatan "Hej, mitt namn är Anna och jag är 25 år gammal." i din terminal eller console.

## Djupdykning:
Att skriva ut debuggade utdata är ett grundläggande felsökningsverktyg som används av programmerare över hela världen. Det finns dock också andra metoder som kan användas, som att använda ett speciellt debuggerverktyg eller loggning av utdata till en fil.

För att implementera utskrift av debuggad utdata i en mer effektiv och organiserad form, kan man skapa en egen loggningsfunktion som tar in specifika parametrar och formaterar utdatan på ett önskat sätt.

## Se även:
[Java API Documentation for System.out.println()](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/PrintStream.html#println())