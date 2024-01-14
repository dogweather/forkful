---
title:                "Clojure: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till gemener (lower case) är en viktig och vanligt förekommande operation inom programmering. Genom att göra detta kan du på ett effektivt sätt hantera och bearbeta data, vilket gör det lättare att skriva skalbara och robusta program.

## Hur man gör
Det finns flera olika sätt att konvertera en sträng till gemener i Clojure. Här är ett exempel på hur du kan göra det med hjälp av inbyggda funktioner:

```Clojure
(def text "HEJ, VÄRLDEN!")
(clojure.string/lower-case text)

;output: hej, världen!
```

Du kan också använda dig av Java's `toLowerCase`-metod genom att omvandla strängen till en Java String först:

```Clojure
(.toLowerCase (String. text))

;output: hej, världen!
```

Det finns också många andra bibliotek och funktioner som kan hjälpa dig att konvertera strängar till gemener, såsom `clojure.string/replace` och `clojure.string/replace-first`. Prova dig fram och hitta den metod som passar dina behov bäst.

## Djupdykning
När du konverterar en sträng till gemener är det viktigt att tänka på teckenkodning (character encoding), särskilt om dina strängar innehåller icke latinska tecken. I dessa fall kan det vara bättre att använda sig av en Unicode-invariant funktion som `clojure.string/lower-case*` istället för den vanliga `clojure.string/lower-case`.

En annan detalj att tänka på är att `clojure.string/lower-case` endast konverterar bokstäver till gemener, så alla andra tecken kommer fortfarande att vara i samma case som innan. Om du vill konvertera hela strängen till gemener, inklusive tecken, kan du använda dig av en kombination av `clojure.string/lower-case` och `clojure.string/replace`.

## Se även
- [Clojure.string API](https://clojure.github.io/clojure/clojure.string-api.html)
- [Java String Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Unicode-invariant functions in Clojure](https://clojure.org/reference/strings)