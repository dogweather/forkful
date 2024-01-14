---
title:    "Clojure: Att skriva till standardfel"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel är ett användbart verktyg för felsökning och debugging när man programmerar i Clojure. Genom att skriva meddelanden till standardfelkanalen kan man få mer detaljerad information om eventuella fel i sin kod.

## Så här gör du

För att skriva ett meddelande till standardfel i Clojure, används funktionaliteten "println" tillsammans med "System/err". Här är ett exempel på hur man skulle skriva ett felmeddelande till standardfel:

```Clojure
(println "Det här är ett felmeddelande till standardfel" (System/err))
```

När man kör koden ovan kommer följande utskrift att visas:

```
Det här är ett felmeddelande till standardfel java.io.PrintStream@12345678
```

Det är viktigt att notera att det andra argumentet till "println" är "(System/err)". Detta instruerar Clojure att skriva meddelandet till standardfelkanalen istället för standardutskriften.

## Djupdykning

Standardfelkanalen i Clojure är en av de tre standardkanalerna som finns tillgängliga för utskrift. De andra två är standardutskriften och standardinmatningen. En intressant egenskap hos standardfelkanalen är att den kan omdirigeras till en annan kanal, till exempel en loggfil. Genom att omdirigera standardfelkanalen kan man få en bättre struktur för felhantering och loggning i sin kod.

Det är också värt att notera att "System/err" är en statisk referens som är tillgänglig för alla Clojure program. Detta gör det möjligt att använda standardfelkanalen i flera olika delar av koden utan att behöva definiera den varje gång.

## Se även

- [Clojure Dokumentation om Standard Error](https://clojure.org/reference/standard_error)
- [Tutorial: Felsökning i Clojure](https://www.braveclojure.com/debugging/)