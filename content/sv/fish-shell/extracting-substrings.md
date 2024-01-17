---
title:                "Utvinna delsträngar"
html_title:           "Fish Shell: Utvinna delsträngar"
simple_title:         "Utvinna delsträngar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar (delsträngar) innebär att man hämtar en del av en större sträng. Detta är ett vanligt behov bland programmerare när man vill manipulera eller behandla specifika delar av en text. Det kan till exempel vara för att utföra sökningar, ersättningar eller för att parsar data.

## Hur man gör:
```Fish Shell``` har inbyggda funktioner som gör det enkelt att extrahera substrängar. Här är ett exempel där vi extraherar en del av en URL:
```
set url "https://www.example.com/article/1234"
set article_number (string sub $url 29 -1)
echo $article_number
```
Output:
```
1234
```
Vi använder ```set``` för att tilldela strängen "https://www.example.com/article/1234" till variabeln url. Sedan använder vi ```string sub``` för att extrahera en del av strängen, från index 29 till slutet av strängen. Slutligen använder vi ```echo``` för att skriva ut den extraherade delen.

## Djupdykning:
Funktionen ```string sub``` har funnits sedan Fish Shell 2.3 och är en väldigt kraftfull och användbar funktion när det kommer till strängmanipulation. Det finns dock också alternativ som kan användas för att extrahera substrängar, såsom ```awk``` och ```sed```. För att implementera substrängsextraktion använder Fish Shell en C-funktion som heter ```fish__sub_string```.

## Se även:
* [Fish Shell dokumentation för string sub](https://fishshell.com/docs/current/cmds/set.html#string_sub)
* [Fish Shell GitHub](https://github.com/fish-shell/fish-shell)