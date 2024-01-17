---
title:                "Extrahering av substrängar"
html_title:           "Clojure: Extrahering av substrängar"
simple_title:         "Extrahering av substrängar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar handlar om att ta bort en del av en sträng och returnera den som en separat sträng. Programmörer använder detta för att manipulera data och få fram specifika delar av en sträng som de behöver.

## Hur man gör:
Exempel 1: Extrahera en substräng från index 2 till 4 från strängen "Hej!"
```Clojure
(subs "Hej!" 2 4)
```
Output: "j!"

Exempel 2: Extrahera en substräng baserad på ett villkor, där endast tecken som är siffror behålls från strängen "abc123xyz"
```Clojure
(re-seq #"\d" "abc123xyz")
```
Output: ("1" "2" "3")

## Djupdykning:
Extrahering av substrängar har funnits under lång tid inom programmering, och används ofta tillsammans med andra strängmetoder för att manipulera och utforska data. Det finns även andra sätt att extrahera substrängar, såsom att använda RegExp-uttryck eller inbyggda strängmetoder.

## Se även:
-https://www.clojure.org/guides/learn/syntax#_strings
-https://www.tutorialspoint.com/clojure/clojure_strings.htm