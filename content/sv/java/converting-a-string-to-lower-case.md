---
title:                "Omvandla en sträng till gemener"
html_title:           "Java: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera en sträng till små bokstäver kan vara väldigt användbart i vissa programmeringsscenarier. Till exempel kan det vara användbart när du jämför strängar eller när du får indata från en användare och vill bearbeta det på ett enhetligt sätt.

## Hur man gör

Det finns ett enkelt sätt att konvertera en sträng till små bokstäver i Java genom att använda metoden "toLowerCase()". Nedan följer ett exempel på hur du kan använda detta i din kod:

```Java
String name = "Jag Är En Sträng";
System.out.println(name.toLowerCase());
```

Detta kommer att ge följande utmatning: "jag är en sträng". Metoden "toLowerCase()" returnerar en ny sträng med alla tecken i små bokstäver.

## Djupdykning

I Java är alla tecken representerade av numeriska värden enligt Unicode-standarden. Bokstäverna A-Z har värdena 65-90 och a-z har värdena 97-122. När man använder metoden "toLowerCase()" i Java jämförs varje teckens numeriska värde för att avgöra om det ska konverteras till en små bokstav. Om det redan är en små bokstav behålls det oförändrat.

Det är också viktigt att notera att "toLowerCase()" bara fungerar på strängar med bokstäver från det engelska alfabetet. Om en sträng innehåller specialtecken eller bokstäver från andra alfabet, kommer de inte att påverkas av metoden.

## Se även

- [Javas officiella dokumentation om string-klassens metoder](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [En annan artikel om strängkonvertering i Java](https://www.baeldung.com/java-string-to-lowercase)
- [En generell guide för Java-programmerare](https://www.tutorialspoint.com/java/index.htm)