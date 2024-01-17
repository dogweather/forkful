---
title:                "Att arbeta med csv"
html_title:           "Clojure: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV (Comma Separated Values) är ett vanligt filformat som används för att lagra tabulära data, såsom en tabell med rader och kolumner. Det är vanligt förekommande för att dela och importera data mellan olika program eller för att spara data i ett enkelt och läsbart format. Programmare använder CSV-filer för att lagra och behandla data på ett effektivt sätt.

## Hur man:
Det finns flera sätt att arbeta med CSV i Clojure, beroende på vilka specifika behov och krav du har. En enkel och vanlig metod är att använda biblioteket "clojure.data.csv" för att läsa och skriva CSV-filer. Här är ett exempel på hur man läser en CSV-fil och skriver ut dess innehåll på skärmen:

```clojure
(require '[clojure.data.csv :as csv])

(with-open [reader (csv/open "data.csv")]
  (doseq [row (csv/read-csv reader)]
    (println row)))
```

Detta kommer att skriva ut varje rad i CSV-filen som en vektor, där varje element representerar en kolumn i tabellen.

### Djupdykning:
CSV-filer har funnits sedan 1972 och har använts flitigt som ett enkelt och läsbart sätt att spara strukturerad data. Men det finns också andra alternativ för att lagra och behandla data, såsom JSON och XML. I Clojure kan man använda bibliotek som "cheshire" eller "clojure.data.xml" för att arbeta med dessa format.

När du arbetar med CSV-filer i Clojure är det viktigt att komma ihåg att olika program kan ha olika standarder för hur CSV-filer ska formateras. Detta kan leda till problem när man försöker importera eller exportera data. Det är därför viktigt att noggrant undersöka och anpassa sig efter de specifika format som behövs.

## Se även:
https://clojure.github.io/data.csv/
https://github.com/funcool/clojure-data.xml
https://github.com/dakrone/cheshire