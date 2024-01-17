---
title:                "Sökning och ersättning av text"
html_title:           "Java: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att söka och ersätta text är ett vanligt förekommande arbetsflöde inom programmering. Det innebär helt enkelt att ersätta en viss bit av text med en annan bit. Det kan vara till nytta när man behöver uppdatera en stor mängd kod eller när man vill korrigera felaktig syntax.

## Så här: 
För att söka och ersätta text i Java, används vanligtvis metoden ```String.replace()```. Denna metod tar två parametrar, den första är den ursprungliga strängen och den andra är den nya strängen som den ska ersätta med. Ett enkelt exempel kan se ut så här:

```Java
String original = "Hej världen!";
String ersatt = original.replace("Hej", "Hallo");
System.out.println(ersatt);
```
> Output: Hallo världen!

## Djupdykning: 
Söka och ersätta text är ett grundläggande koncept inom programmering och har funnits sedan de tidigaste dagarna av datorer. Alternativa sätt att utföra samma uppgift inkluderar Regex-uttryck och användning av andra Java-strängmetoder som ```String.replaceAll()```.

Det är också viktigt att vara försiktig vid sökning och ersättning av text, eftersom det kan ha oavsiktliga konsekvenser på annan kod som använder samma ord eller fraser. Dessutom bör man vara noga med att använda rätt matchningsmönster så att endast önskade delar av texten ersätts.

## Se även: 
Läs mer om sökning och ersättning av text i Java på [Oracles officiella dokumentation](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#replace(java.lang.CharSequence,%20java.lang.CharSequence)). Om du är intresserad av Regex-uttryck, kan du läsa mer på [denna resurs](https://www.javatpoint.com/java-regex).