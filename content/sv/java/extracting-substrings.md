---
title:                "Java: Extraherande av delsträngar"
simple_title:         "Extraherande av delsträngar"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Att extrahera substrängar är en vanligt använd kodningsteknik för många Java-programmerare. Det är ett sätt att ta en del av en sträng och använda den för att utföra specifika uppgifter eller manipulera data på ett enklare sätt.

## Hur man gör det
Det finns flera olika sätt att extrahera substrängar i Java, men det vanligaste är att använda ```substring()```-metoden. Här är ett exempel på hur man kan använda det:

```Java
String str = "Välkommen till Java-programmering";
String substr = str.substring(11, 24);
System.out.println(substr);
```

Output: "Java-programmering"

I detta exempel används ```substring()```-metoden för att extrahera delen av strängen som börjar på index 11 och slutar på index 24. Det är viktigt att komma ihåg att indexet börjar på 0 i Java, så det första tecknet i strängen har index 0.

En annan användbar metod för att extrahera substrängar är ```split()```. Det här är särskilt användbart om du vill dela upp en sträng i flera mindre delar baserat på ett visst tecken eller en sekvens av tecken.

```Java
String str = "Göteborg, Stockholm, Malmö, Uppsala, Lund";
String[] substrings = str.split(", ");

for (String s : substrings) {
    System.out.println(s);
}
```

Output: 
"Göteborg"
"Stockholm"
"Malmö"
"Uppsala"
"Lund"

## Djupdykning
När du arbetar med substrängar i Java finns det några saker att tänka på. Först och främst är det viktigt att komma ihåg att en sträng är en oföränderlig datatype i Java, vilket betyder att du inte kan ändra på den ursprungliga strängen när du extraherar en substräng. Istället kommer en ny sträng att skapas med den extraherade delen.

En annan viktig sak att komma ihåg är att indexet i en sträng alltid börjar på 0 och går upp till strängens längd minus 1. Om du försöker använda ett index som är utanför den tillåtna gränsen kommer du att få ett ```IndexOutOfBoundsException```.

Ytterligare en användbar metod för att arbeta med substrängar är ```startsWith()```. Detta kan användas för att kontrollera om en sträng börjar med en viss sekvens av tecken eller inte.

```Java
String str = "Hello World!";
if (str.startsWith("Hello")) {
    System.out.println("Strängen börjar med 'Hello'.");
}
```

Output: "Strängen börjar med 'Hello'."

## Se även
Här är några länkar till ytterligare resurser för dig som vill lära dig mer om att extrahera substrängar i Java:

- [Java String Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java substring() Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-)
- [Java split() Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#split-java.lang.String-)
- [Java startsWith() Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#startsWith-java.lang.String-)