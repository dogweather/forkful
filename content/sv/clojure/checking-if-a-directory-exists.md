---
title:                "Kontrollera om en mapp existerar"
html_title:           "Clojure: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp existerar är en viktig del av programmering eftersom det hjälper till att hantera filer och strukturera koden på ett effektivt sätt. Detta är särskilt användbart när man arbetar med stora projekt som har många filer och mappar.

## Hur man gör:
För att kontrollera om en mapp existerar i Clojure kan du använda funktionen "exists?" från standardbiblioteket "clojure.java.io". Här är ett exempel på hur den kan användas:

```Clojure
(require '[clojure.java.io :as io])
(io/exists? "/mapp_namn")
```

Om mappen existerar kommer funktionen att returnera sann, annars returneras falskt.

## Djupdykning:
I äldre versioner av Clojure användes funktionen "file-exists?" från "clojure.contrib.io" för att kontrollera mappar, men den har nu ersatts av "exists?" från standardbiblioteket. Det finns också alternativ som "file-seq" och "walk" som kan användas för att iterera genom en mapp och dess undermappar. Funktionen "exists?" använder sig av Java-biblioteket "File" för att utföra kontrollen.

## Se också:
Officiell dokumentation för "clojure.java.io" - https://clojure.github.io/clojure/clojure.java.io-api.html  
Exempel på användning av "exists?" från Clojure for the Brave and True - https://www.braveclojure.com/files-directories/