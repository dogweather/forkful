---
title:                "Taletiserande en sträng"
html_title:           "Clojure: Taletiserande en sträng"
simple_title:         "Taletiserande en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför & Hur?
Att ”sätta stora bokstäver” på en sträng innebär helt enkelt att göra alla bokstäver i strängen till stora (versaler) istället för små (gemener). Program  gör detta för att till exempel skapa en mer enhetlig och estetiskt tilltalande presentation av data, eller för att underlätta jämförelse av strängar.

## Så här gör du:
```Clojure 
(def string "hello world")
(str/upper-case string)
```
Output: "HELLO WORLD"

## Djupdykning:
Att sätta stora bokstäver på en sträng har länge varit ett vanligt koncept inom programmering och är inte unikt för Clojure. Många programmeringsspråk har dedikerade funktioner för detta ändamål, som till exempel "toUpper()" i Java. Det finns också alternativa sätt att åstadkomma samma resultat, som att använda strängmanipuleringsfunktioner eller reguljära uttryck.

I Clojure finns en inbyggd funktion, "upper-case", i standardbiblioteket för att omvandla en sträng till versaler. Denna funktion är inte bara begränsad till engelska alfabetet, utan fungerar för alla unicode-tecken.

## Se även:
- [Clojure Core Library](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/upper-case)
- [Java String API](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [Regular Expressions](https://regexr.com/)