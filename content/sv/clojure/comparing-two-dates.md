---
title:                "Jämföring av två datum"
html_title:           "Clojure: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vad och varför?
När vi programmerar, kan vi behöva jämföra två datum för att utföra olika beräkningar eller för att bestämma om ett datum är före eller efter ett annat. Att jämföra datum är en vanlig uppgift för många programmerare.

# Hur gör man:
För att jämföra två datum i Clojure, kan vi använda den inbyggda funktionen `clojure.core/compare`. Denna funktion tar två datum som argument och returnerar -1 om det första datumet är tidigare än det andra, 0 om de är lika och 1 om det första datumet är senare än det andra. 

```Clojure
(clojure.core/compare "2021-03-15" "2021-03-20") ; returnerar -1
(clojure.core/compare "2021-03-15" "2021-03-15") ; returnerar 0
(clojure.core/compare "2021-03-20" "2021-03-15") ; returnerar 1
```

Vi kan också använda funktionen `java.util.Date` för att konvertera en sträng till ett datumobjekt och sedan jämföra dessa två objekt med `clojure.core/compare`.

```Clojure
(def date1 (java.util.Date. "2021-03-15"))
(def date2 (java.util.Date. "2021-03-20"))

(clojure.core/compare date1 date2) ; returnerar -1
```

# Öka förståelsen:
Det finns flera alternativ för att jämföra datum i Clojure, såsom att använda biblioteket `clj-time` eller `java.time` som introducerades i Java 8. Med `clj-time` biblioteket kan vi använda funktionen `clj-time.core/compare` för att jämföra två datum.

När vi jämför datum i Clojure, är det viktigt att komma ihåg att vi använder Clojures inbyggda funktioner och strukturer för att hantera och manipulera datumobjekt. Det är också värt att notera att `java.time` biblioteket är mer optimerat för prestanda än `clj-time` och bör övervägas vid hantering av stora datamängder.

# Se även:
- [Clojure Dokumentation om Datatyper](https://clojure.org/reference/data_structures)
- [clj-time biblioteks dokumentation](https://github.com/clj-time/clj-time)
- [java.time dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)