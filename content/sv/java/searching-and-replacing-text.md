---
title:    "Java: Sökning och ersättning av text"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Att söka och ersätta text är en vanlig uppgift i programmering och kan hjälpa till att effektivisera och förbättra koden. Genom att lära sig denna färdighet kan du enkelt göra stora ändringar i din kod, spara tid och undvika mänskliga fel.

## Så här gör du
Det finns flera sätt att söka och ersätta text i Java, men det mest grundläggande sättet är med hjälp av `String`-klassens `replace()`-metod. Du kan använda den på följande sätt:

```Java
String originalText = "Detta är en textsträng.";
String newText = originalText.replace("en", "ett");

System.out.println(newText); // Utskrift: Detta är ett textsträng.
```
Som du kan se har vi ersatt "en" med "ett" i den ursprungliga textsträngen. Om du vill ersätta alla förekomster av en viss text kan du använda `replaceAll()`-metoden istället.

För att söka efter ett specifikt mönster i en textsträng kan du använda `Pattern` och `Matcher`-klasserna. Här är ett exempel på hur du kan söka efter alla siffror i en textsträng och ersätta dem med en @-symbol:

```Java
String originalText = "Det finns 123 äpplen i korgen.";
String newText = originalText.replaceAll("\\d", "@");

System.out.println(newText); // Utskrift: Det finns @@@ äpplen i korgen.
```
I detta exempel använder vi reguljära uttryck för att söka efter alla siffror och ersätta dem med en @-symbol.

## Djupdykning
När du söker och ersätter text i Java finns det några saker som är viktiga att tänka på. Till exempel är `replace()` och `replaceAll()`-metoderna mycket känsliga för skillnader i gemener och versaler. Det är också viktigt att komma ihåg att dessa metoder returnerar en ny sträng och inte ändrar den ursprungliga strängen.

Det finns också andra metoder som är specifika för olika typer av objekt, som `StringBuilder` och `StringBuffer`, som kan användas för att modifiera en textsträng på plats istället för att skapa en ny kopia.

## Se även
- [Java String-klassen](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Pattern-klassen](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Java Matcher-klassen](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Matcher.html)