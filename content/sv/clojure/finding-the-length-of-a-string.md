---
title:                "Att hitta längden på en sträng"
html_title:           "Clojure: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng handlar om att räkna antalet tecken i en viss text. Programerare gör detta för att det är ett vanligt steg i att bearbeta och manipulera textdata.

## Hur man gör det:

```Clojure
(def minSträng "Hej, världen!")
(count minSträng)
```
```
Output: 13
```

```Clojure
(def annanSträng "Detta är ett längre exempel på en sträng")
(count annanSträng)
```
```
Output: 39
```

## Djupdykning:

Att hitta längden på en sträng är en grundläggande strängmanipulation i Clojure och är en del av standardbiblioteket. Det finns dock alternativ som kan vara mer relevanta beroende på vilken typ av data som bearbetas. Det är viktigt att notera att längden av en sträng kan skilja sig åt beroende på vilken teckenkodning som används.

## Se även:

Officiell dokumentation för strängmanipulation i Clojure: https://clojure.org/reference/strings
Alternativa metoder för att hitta längden på en sträng i Clojure: https://www.programcreek.com/2012/12/find-the-length-of-a-string-in-clojure/