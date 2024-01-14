---
title:                "Clojure: Att skriva till standardfel"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standardfel (standard error) är ett användbart verktyg vid debuggning av din Clojure-kod. Genom att skriva felmeddelanden till standardfel kan du enkelt hitta och åtgärda problem i din kod.

## Hur man gör det
För att skriva till standardfel, kan du använda funktionen "System/err-print" eller "System/err-println". Dessa funktioner tar emot en sträng som argument och skriver den till standardfel. Se nedan för exempel och output.

```Clojure
(System/err-print "Detta är ett felmeddelande")
(System/err-println "Detta är ett annat felmeddelande")

```
Output:
Detta är ett felmeddelandeDetta är ett annat felmeddelande

## Djupdykning
Vad är standardfel egentligen? Det är en ström (stream) som används för felutmatning i Java-program. När du skriver till standardfel i Clojure, skickas det faktiskt till standardfelströmmen i JVM.

Att skriva till standardfel är en användbar funktion vid debuggning, eftersom den ger dig möjlighet att separera utmatning av felmeddelanden från vanlig utmatning på standardutmatningsströmmen (standard output stream).

## Se även
- [Clojure Dokumentation om standardfel](https://clojuredocs.org/clojure.core/system/err-print)
- [Java Dokumentation om standardfel](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
- [Blogginlägg om felhantering i Clojure](https://www.lambdaisland.com/blog/2018-01-30-exception-handling-clojure)