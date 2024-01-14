---
title:    "Clojure: Skapa en tillfällig fil"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför skapa en tillfällig fil i Clojure?

Att skapa tillfälliga filer kan vara användbart i flera olika situationer, till exempel när man behöver spara temporär data eller när man behöver skapa en temporär fil för en viss operation. I Clojure finns det flera enkla och effektiva sätt att skapa tillfälliga filer, vilket vi kommer att utforska i denna bloggpost.

## Hur man skapar en tillfällig fil i Clojure

För att skapa en tillfällig fil i Clojure kan man använda funktionen `with-open` tillsammans med `java.io.File/createTempFile` för att skapa en temporär fil med ett unikt namn. Detta kan se ut på följande sätt i koden:

```Clojure 
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")] 
  (println "En tillfällig fil skapad med namnet " (.getName temp-file))) 
```

Output: En tillfällig fil skapad med namnet temp7561119441009090568.txt

Här har vi skapat en tillfällig fil med namnet "temp7561119441009090568.txt". Det är viktigt att notera att filen kommer att bli automatiskt borttagen när `with-open` funktionen avslutas.

## Gräva djupare

Det finns flera olika argument som kan användas med `java.io.File/createTempFile` för att skräddarsy den skapade tillfälliga filen. Till exempel kan man ange en specifik katalog där filen ska skapas, eller lägga till ett prefix eller suffix till filnamnet. Man kan även använda `java.io.File/deleteOnExit` för att ange att filen ska tas bort när programmet avslutas.

Det finns också flera andra sätt att hantera tillfälliga filer i Clojure, såsom att använda `java.io.File/createNewFile` för att skapa en ny fil eller `clojure.java.io/resource` för att skapa en fil med resursnamn. Mer information om dessa alternativ och deras funktioner finns tillgängliga i Clojure-dokumentationen.

## Se även

- [Clojure documentation on temporary files](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/resource)
- [How to work with temporary files in Java](https://www.baeldung.com/java-temporary-files)
- [Tutorial on file handling in Clojure](https://alive.cz/news/alive-hub-article-file-handling-in-clojure/)