---
title:                "Omvandla en sträng till gemener"
html_title:           "Clojure: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är en vanlig uppgift i programmering, eftersom det hjälper till att standardisera data och göra det enklare att jämföra strängar.

## Så här gör du

```Clojure
(.toLowerCase "STRING") ; "string"
(.toLowerCase "ÄPPLE") ; "äpple"
(.toLowerCase "HeLlO WoRlD") ; "hello world"
```

Som du ser används metoden `.toLowerCase` för att konvertera en sträng till gemener. Metoden kan appliceras på alla typer av strängar, oavsett om de innehåller bokstäver, siffror, specialtecken eller en blandning av allt.

## Djupdykning

Konvertering till gemener är en viktig del av Unicode-standardens normalization, vilket innebär att alla tecken representeras på ett standardiserat sätt. Detta underlättar för jämförelse av text på olika språk och teckenkodningar.

## Se även

- [Official Clojure Documentation: Characters and Strings](https://clojure.org/reference/characters)
- [Clojure Cheat Sheet: Strings](https://clojure.org/api/cheatsheet#Strings)