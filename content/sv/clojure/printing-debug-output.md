---
title:                "Clojure: Utmatning av felsökningsresultat"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debuggutdata är ett viktigt verktyg för att felsöka och förstå hur koden fungerar. Genom att skriva ut olika värden och variabler kan du se vilka värden som tilldelas och om de är korrekta. Detta hjälper till att lösa problem och förbättra koden.

## Så här gör du

För att skriva ut debuggutdata i Clojure, använder du funktionen `println`. Detta utskriftsverktyg tar emot en eller flera argument och skriver ut dem i terminalen.

```Clojure
(println "Detta är en testtext") ; Output: Detta är en testtext
```

Du kan också skriva ut flera värden samtidigt genom att lägga till dem som separata argument till `println`.

```Clojure
(def name "Alice")
(def age 25)
(println "Namn:" name "Ålder:" age) ; Output: Namn: Alice Ålder: 25
```

Du kan också använda funktionen `prn` för att skriva ut värden utan att lägga till en ny rad efter utskriften.

```Clojure
(prn "Detta är" "en testtext") ; Output: "Detta är" "en testtext"
```

## Djupdykning

Det finns olika användbara tekniker för att använda `println` och `prn` för att skriva ut debuggutdata. En av dessa är att använda dem inuti en `let` sats för att skriva ut värden från en specifik del av koden.

```Clojure
(let [x 5 y 10]
  (println "Summan av x och y är" (+ x y))) ; Output: Summan av x och y är 15
```

En annan användbar teknik är att använda `with-out-str` för att få utskriften som en sträng istället för att skriva direkt till terminalen. Detta kan vara särskilt användbart när du vill skriva ut värden från ett mer komplicerat datastrukturer som en vektor eller en hashmap.

```Clojure
(def my-vector [1 true "test"])
(with-out-str (prn my-vector)) ; Output: "[1 true "test"]"
```

## Se även

* [Official Clojure documentation for printing](https://clojuredocs.org/clojure.core/print)
* [Tutorial: debugging in Clojure](https://purelyfunctional.tv/guide/debugging-in-clojure/)