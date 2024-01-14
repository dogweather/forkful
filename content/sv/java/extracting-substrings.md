---
title:    "Java: Extrahera substrängar"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför

När vi programmerar i Java, finns det ofta situationer där vi behöver extrahera en del av en textsträng för att använda den på ett annat ställe. Det kan vara för att söka efter ett visst mönster, manipulera data eller helt enkelt för att organisera informationen på ett bättre sätt. Att extrahera substrings är ett vanligt problem som många möter under sin programmeringskarriär och är därför en viktig färdighet att behärska.

## Så här gör man

För att extrahera substrings i Java, använder vi metoden `substring()` från klassen `String`. Denna metod tar två parametrar: startpositionen och slutpositionen för den del av strängen som vi vill extrahera. Det är viktigt att notera att den slutliga positionen inte inkluderas i substringen.

```
// Skapa en String-variabel med texten "Hello World"
String text = "Hello World";

// Extrahera första ordet "Hello" med hjälp av start- och slutpositionen
String hello = text.substring(0, 5);

// Skriv ut resultatet
System.out.println(hello); // Output: Hello
```

Vi kan också extrahera substrings baserat på ett visst mönster, såsom en viss bokstav eller tecken. För detta använder vi metoden `indexOf()` för att hitta positionen för det specifika mönstret och sedan anger vi denna position som startposition i `substring()`.

```
// Skapa en String-variabel med texten "Java is awesome"
String text = "Java is awesome";

// Extrahera "awesome" genom att söka efter mönstret "is"
String awesome = text.substring(text.indexOf("is") + 3);

// Skriv ut resultatet
System.out.println(awesome); // Output: awesome
```

## Djupdykning

När vi använder metoden `substring()` är det viktigt att vara medveten om att den ursprungliga strängen är oförändrad. Det betyder att vi inte kan ändra på eller ta bort delar av den ursprungliga strängen genom att extrahera en substring. För att modifiera en sträng måste vi först skapa en ny sträng med de önskade ändringarna.

En annan viktig aspekt att tänka på är att positionerna för substrings räknas från noll, vilket betyder att den första bokstaven i strängen har positionen 0. Om vi vill extrahera en del av strängen från en viss position till slutet, kan vi använda metoden `substring()` utan att ange en slutposition.

## Se även

- [Java String API](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java String Class Tutorial](https://www.w3schools.com/java/java_string.asp)
- [Substring in Java](https://www.geeksforgeeks.org/substring-java/)