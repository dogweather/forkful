---
title:                "Stor bokstavering av en sträng"
html_title:           "Java: Stor bokstavering av en sträng"
simple_title:         "Stor bokstavering av en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att göra en sträng till stor bokstav innebär helt enkelt att förvandla varje första bokstav i varje ord till en stor bokstav. Programmerare gör detta för att hålla en konsekvent och läsbar kod, samt för att möta utformningsstandarder.

## Så här gör man:
```Java
String str = "detta är en sträng som ska bli stor bokstav";
str = str.toUpperCase();
System.out.println(str);
```
Output: DETTA ÄR EN STRÄNG SOM SKA BLI STOR BOKSTAV

## Djupdykning:
Att förvandla en sträng till stor bokstav har funnits sedan programmeringens tidiga dagar, och är fortfarande en viktig del av kodning. Det finns olika sätt att göra detta, men den enklaste och vanligaste är att använda metoden `toUpperCase()`. Det finns också alternativ som till exempel Regular Expressions, men dessa kan vara mer komplicerade att använda. I Java, är `toUpperCase()` en del av klassen `String`, vilket gör det lätt att använda i din kod.

## Se även:
För mer information om att förvandla en sträng till stor bokstav, se Java:s officiella dokumentation: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--