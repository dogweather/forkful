---
title:    "Clojure: Kontrollera om en mapp finns"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en katalog existerar kan vara en viktig del av en Clojure-programmerares arbetsflöde. Genom att förstå hur man gör detta kan du enkelt hantera filer och kataloger i dina projekt.

## Hur man gör
För att kontrollera om en katalog existerar i Clojure kan du använda funktionen ```clojure.file/exists?```. Detta fungerar även för att kontrollera om en fil existerar.

```Clojure
(def directory (str "./example-directory")) ; Skapar en sträng som representerar katalogen

(clojure.file/exists? directory) ; Kontrollerar om katalogen existerar

; Om katalogen existerar kommer utskriften att vara true, annars false
```

Om du vill ta reda på om en katalog existerar med absolut sökväg istället kan du använda ```java.nio.file.Path``` och dess ```exists```-funktion.

```Clojure
(def directory (java.nio.file.Paths/get "./example-directory")) ; Skapar sökvägen till katalogen

(.exists directory) ; Kontrollerar om katalogen existerar

; Om katalogen existerar kommer utskriften att vara true, annars false
```
De två metoderna ovan fungerar på liknande sätt men använder olika typer av sökvägar. Det är viktigt att välja den som passar bäst för ditt ändamål.

## Djupdykning
För att förstå hur Clojure kan kontrollera om en katalog existerar, är det viktigt att förstå hur katalogstrukturer och filhantering fungerar i JVM. Clojure använder Java-bibliotek för filhantering, vilket ger tillgång till ett brett utbud av funktioner för att hantera filer och kataloger.

Funktionen ```clojure.file/exists?``` returnerar en boolean-variabel, vilket innebär att den endast ger ut ett svar på om katalogen existerar eller inte. Om du behöver ytterligare information om katalogen, som till exempel när den senaste ändringen gjordes, kan du använda funktionen ```java.nio.file.Files/lastModifiedTime```.

```Clojure
(def directory (java.nio.file.Paths/get "./example-directory")) ; Skapar sökvägen till katalogen

(def last-modified (.lastModifiedTime (java.nio.file.Files/getLastModifiedTime directory))) ; Hämtar datumet för senaste ändringen

(last-modified :toInstant) ; Skriver ut datumet i en mer läsbar format
```

Genom att förstå dessa funktioner kan du göra mer avancerad filhantering i dina Clojure-projekt.

## Se även
- [Clojure filreferens](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [JVM filhantering](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)