---
title:    "Clojure: Radera tecken som matchar ett mönster"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara en användbar färdighet för programmerare som behöver manipulera strängar eller andra datastrukturer. Genom att lära sig hur man gör det kan du förenkla och effektivisera din kod.

## Så här gör du

```Clojure
;; För att ta bort tecken som matchar ett specifikt mönster, kan du använda funktionen "remove". Till exempel, om du vill ta bort alla siffror från en sträng kan du använda följande kod:
(remove #(Character/isDigit %) "hej123värld")
;; Output: "hejvärld"

;; För att ta bort tecken från en datastruktur, t.ex. en vektor, kan du använda "filter" funktionen tillsammans med "remove". Till exempel:
(remove #(Character/isLetter %) (filter #(Integer/isInteger %) [1 2 "a" 3 "b"]))
;; Output: [1 2 3]
```

## Djupdykning

Funktionen "remove" returnerar en ny datastruktur utan de matchande tecknen. Det är också möjligt att använda den på en sekvens, som en sträng, för att ta bort en del av den. Detta kan vara användbart för att till exempel filtrera ut ogiltig input eller städa upp formatering i en sträng.

Det finns också andra funktioner som kan användas för att ta bort tecken som matchar ett mönster, som "re-seq" eller "re-find". Dessa kan vara användbara beroende på ditt specifika projekt eller behov.

## Se även

- [Clojure Dokumentation för funktionen remove](https://clojuredocs.org/clojure.core/remove)
- [Clojure Cookbook för att ta bort tecken från strängar](https://clojure-cookbook.com/strings/parsing_strings.html#parsing-strings-_removing_characters_from_strings)