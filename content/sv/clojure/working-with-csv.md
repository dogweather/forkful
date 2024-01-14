---
title:                "Clojure: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

# Varför

 CSV-formatet, som används för att lagra tabellbaserad data i textfiler, är ett vanligt sätt att dela och hantera data inom programutveckling. Genom att lära sig att arbeta med CSV-filer i Clojure kan du effektivt hantera och manipulera data i dina program.

## Hur man gör

Att läsa och skriva till CSV-filer i Clojure är enkelt med hjälp av Clojure's inbyggda CSV-bibliotek, som finns tillgängligt i nyare versioner av Clojure. För att läsa in en CSV-fil i en vektor, använd funktionen `clojure.data.csv/read-csv` och ange filvägen som argument. Här är ett enkelt exempel som läser in en CSV-fil med namn och åldrar och skriver ut den:

```Clojure
(ns min-namespace
  (:require [clojure.data.csv :as csv]))

; Läs in filen och spara i en vektor
(def data (csv/read-csv "data.csv"))

; Loopa igenom vektorn och skriv ut namn och ålder för varje person
(doseq [[namn ålder] data]
  (println (str namn " är " ålder " år gammal.")))

```

**Utskrift:** 

> Erik är 30 år gammal.
> Sara är 25 år gammal.
> Johan är 45 år gammal.

Att skriva till en CSV-fil är lika enkelt. Använd funktionen `clojure.data.csv/write-csv` och ange en filväg och en vektor med data som argument. Här är ett exempel på hur man skulle skriva till en ny CSV-fil med namn och löner:

```Clojure
(ns min-namespace
  (:require [clojure.data.csv :as csv]))

; Skapa en vektor med data
(def data [["Erik" 50000]
           ["Sara" 60000]
           ["Johan" 75000]])

; Skriv till en ny CSV-fil
(csv/write-csv "löner.csv" data)
```

**CSV-fil:**

> Erik, 50000
> Sara, 60000
> Johan, 75000

## Djupdykning

Det finns flera andra funktioner inom CSV-biblioteket som kan vara användbara beroende på dina behov. Till exempel kan du använda funktionen `flatten-csv-row` för att omvandla en CSV-rad till en vektor med nyckel-värde-par baserat på rubrikerna i CSV-filen. Eller så kan du använda `write-csv-with-headers` för att skriva till en CSV-fil med rubriker som definieras i en vektor.

För att lära dig mer om hur du kan arbeta med CSV-filer i Clojure, rekommenderar vi att du läser dokumentationen för CSV-biblioteket och testar olika funktioner och exempel.

# Se även

- [Officiell Dokumentation för CSV-biblioteket](https://clojure.github.io/java.csv/javadoc/clojure/data/csv/package-summary.html)
- [Clojure Style Guide för CSV](https://github.com/bbatsov/clojure-style-guide#csv)