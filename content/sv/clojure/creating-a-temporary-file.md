---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skapa en temporär fil innebär en process där ett datorprogram skapar en fil som behövs för en kort stund, men inte permanent. Programmerare gör detta för olika ändamål, till exempel för att lagra data som behövs under programmets körning, men inte efteråt.

## Hur du gör:
Du kan skapa temporära filer i Clojure med hjälp av `java.io.File`-klassen. Här är ett kort exempel:

```Clojure
(ns temp.example
    (:import
        [java.io File]))

(defn create-temp-file []
    (let [temp-file (File/createTempFile "temp" ".txt")]
        (println (.getPath temp-file))))
```

Om du kör denna kod kommer den att skriva ut sökvägen till den nyligen skapade temporära filen.

## Djupdykning
Historiskt sett uppfanns temporära filer för att hantera storleksbegränsningar på äldre system, men idag används de mest för att lindra lasten på ett systems primärminne.

Det finns alternativ för temporära filer i Clojure. Ett sådant alternativ är att använda ett ramlager eller in-memory datastrukturer. Men det är värt att komma ihåg att dessa alternativ kan ha begränsningar beroende på volymen av data som behandlas.

När det gäller detaljer om implementation, när `File/createTempFile` kallas, skapar JVM en ny, tom fil på filsystemet och returnerar en `File` referens till den här filen. Det är viktigt att notera att den här filen endast är tillgänglig för befintligt program och raderas när JVM stängs av.

## Se Även
Du kan läsa mer om användningen och skapandet av temporära filer i programmering genom följande länkar:

1. [Oracle Java Documentation for File Class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
2. [Clojure Cookbook's Section on Dealing with Files](https://www.braveclojure.com/core-functions-in-depth/)
3. [Stack Overflow's Topics on Temporary Files](https://stackoverflow.com/questions/tagged/temporary-files)