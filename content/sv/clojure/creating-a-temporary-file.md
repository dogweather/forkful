---
title:                "Clojure: Skapa en tillfällig fil"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Skapandet av temporära filer är vanligt inom programmering eftersom det ofta är nödvändigt att lagra eller manipulera tillfälliga data under körningstiden. Det kan också vara användbart för att testa och felsöka kod utan att påverka den befintliga filstrukturen.

## Hur man gör det

För att skapa en temporär fil i Clojure använder man funktionen `with-open`, som tar emot en file handle som argument och ser till att filen stängs efter användning. Sedan kan man använda funktionen `clojure.java.io/file` för att skapa en ny fil och välja dess namn och placering.

```Clojure
(with-open [temp-file (clojure.java.io/file "C:/temp/" "tempfile.txt")]
  (println "Min temporära fil: " (.getName temp-file))
  (.deleteOnExit temp-file)) ; filen raderas automatiskt vid avslutad körning
```

Detta kommer att skapa en temporär fil med namnet "tempfile.txt" i mappen "C:/temp" och sedan skriva ut filnamnet. Funktionen `deleteOnExit` ser till att filen raderas när programmet avslutas.

## Djupdykning

För mer avancerad hantering av temporära filer kan man använda sig av `java.io.File` klassen och dess metoder, som till exempel `isDirectory` och `listFiles` för att undersöka och hantera filer på olika sätt.

Man kan också skapa en temporär fil som endast ska existera under körningstiden genom att använda `java.io.File.createTempFile` funktionen, som automatiskt skapar en fil med ett unikt namn och lägger till ett prefix och suffix.

```Clojure
(with-open [temp-file (java.io.File/createTempFile "prefix_" "_suffix")]
  (println "Min temporära fil: " (.getName temp-file))
  (.deleteOnExit temp-file))
```

## Se även

- [Clojure.io API documentation](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Java File API documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#createTempFile(java.lang.String,java.lang.String,java.nio.file.attribute.FileAttribute...))