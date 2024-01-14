---
title:    "Java: Tar bort tecken som matchar ett mönster"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför
Ibland kan det finnas behov av att ta bort vissa tecken från en textsträng som matchar ett visst mönster. Detta kan vara användbart om man till exempel vill rensa bort alla specialtecken från en text innan den används i en databas eller om man vill bara vill ha kvar siffror i en textsträng för att göra matematiska beräkningar. I denna bloggpost kommer vi att titta på hur man kan göra detta i Java.

## Så här gör du
För att ta bort tecken som matchar ett visst mönster, kan vi använda oss av det inbyggda Java-verktyget "replaceAll()" tillsammans med en regular expression (reguljärt uttryck). Detta är ett kraftfullt verktyg som kan ta emot olika mönster och utföra olika operationer beroende på vad vi vill uppnå.

Låt oss titta på ett exempel där vi vill ta bort alla specialtecken från en textsträng:

```Java
String text = "Hej!Vad har du för favoritfärg?";
String cleanedText = text.replaceAll("[^a-zA-Z0-9]", "");
System.out.println(cleanedText);
```

I detta exempel använder vi "replaceAll()" metoden tillsammans med en reguljär expression "[^a-zA-Z0-9]" som betyder att vi vill ta bort alla tecken som inte är bokstäver (a-z och A-Z) eller siffror (0-9) från vår textsträng. Detta inkluderar alla specialtecken som möts i texten. Vi tilldelar den rensade texten till en ny variabel och sedan skriver ut resultatet, vilket blir "HejVadharduforfavoritfarg".

Vi kan också använda "replaceAll()" för att bara behålla vissa typer av tecken i texten. Till exempel, om vi bara vill ha kvar siffror och punktuationstecken, kan vi använda följande kod:

```Java
String text = "Det finns 3 stolar i rummet.";
String cleanedText = text.replaceAll("[^0-9.,]", "");
System.out.println(cleanedText);
```

I detta exempel använder vi "[^0-9.,]" för att behålla enbart siffror, punktuationstecken och kommatecken. Resultatet blir "3,.".

## Djupdykning
Om du vill lära dig mer om reguljära uttryck och hur de kan användas för att manipulera textsträngar, kan du läsa mer på Java dokumentationen eller andra online-resurser. Det finns många olika mönster och operationer som kan utföras med hjälp av reguljära uttryck, så det är definitivt värt att fördjupa sig i ämnet om du vill bli en expert på textmanipulering i Java.

## Se också
- [Java dokumentation om reguljära uttryck](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Online reguljärt uttrycksgenerator](https://regexr.com/)
- [Java String klassen](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)