---
title:                "Arbeta med csv"
html_title:           "Clojure: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför
Om du arbetar med hantering av stora mängder data, så är CSV (Comma Separated Values) ett vanligt format för att importera och exportera data. Genom att använda Clojure för att hantera CSV-filer kan du effektivt och enkelt manipulera och analysera data.

## Så här gör du
För att börja hantera CSV-filer i Clojure, måste du först importera biblioteket "clojure.data.csv" med hjälp av följande kod:

```Clojure
(require '[clojure.data.csv :as csv])
```

### Läsa in en CSV-fil
För att läsa in en CSV-fil och spara den som en vektor, kan du använda följande kod:

```Clojure
(with-open [file (clojure.java.io/reader "minfil.csv")]
    (doall (csv/read-csv file)))
```

Detta kommer att läsa in filen "minfil.csv" och returnera en vektor där varje rad i CSV-filen representeras som en undervektor.

### Skriva till en CSV-fil
För att skriva data till en CSV-fil kan du använda följande kod:

```Clojure
(with-open [file (clojure.java.io/writer "minfil.csv")]
    (csv/write-csv file [["1" "John" "Doe"]
                         ["2" "Jane" "Smith"]]))
```

Detta kommer att skapa en fil med namnet "minfil.csv" och skriva data från en vektor av undervektorer till filen. Varje undervektor motsvarar en rad i CSV-filen.

### Manipulera data
Med hjälp av Clojure-funktioner som "map", "filter" och "reduce" kan du enkelt manipulera data från CSV-filer. Till exempel kan du konvertera alla nummer i en kolumn till heltal och sedan filtrera ut alla poster som har ett värde lägre än 100:

```Clojure
(with-open [file (clojure.java.io/reader "minfil.csv")]
    (doall (->> (csv/read-csv file)
                (map #(vec (->> (get % 0) (map #(if (re-matches #"\d+" %) (Integer/parseInt %) %)))))
                (filter #(> (nth % 2) 100)))))
```

I detta exempel gör vi följande:

  - Först läser vi in CSV-filen och använder "read-csv" för att returnera en vektor av undervektorer.
  - Sedan använder vi "map" för att konvertera varje värde i den första kolumnen till ett heltal med hjälp av "replace" och "Integer/parseInt".
  - Slutligen filtrerar vi ut alla poster som har ett värde lägre än 100 med hjälp av "filter".

## Djupdykning
När du arbetar med CSV-filer i Clojure, finns det några saker du bör tänka på:

  - Om du har CSV-filer som använder ett annat tecken för att separera värden (t.ex. semikolon), kan du ange detta som ett valfritt argument till "read-csv" och "write-csv".
  - Om en CSV-fil innehåller strängar som börjar eller slutar med citattecken, kommer "read-csv" att returnera dessa som strängar med citattecken. Detta kan orsaka problem vid senare manipulation av data, så det är viktigt att ta hänsyn till detta vid hantering av CSV-filer.
  - Om du behöver hantera stora CSV-filer, kan det vara ineffektivt att läsa hela filen i minnet på en gång. Istället kan du använda "with-open" tillsammans med "csv/parse-csv" för att läsa och hantera en rad i taget.

## Se även
  - [Clojure Dokumentation](https://clojure.org/)
  - [ClojureFest EU](https://clojurefest.eu/)
  - [DatafyClj - Ett bibliotek för att hantera data från CSV-filer i Clojure](https://github.com/rafaelrinaldi/datafy-clj)