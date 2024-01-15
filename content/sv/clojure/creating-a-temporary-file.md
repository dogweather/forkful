---
title:                "Skapa en temporär fil"
html_title:           "Clojure: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Det finns många olika anledningar till att skapa en temporär fil i din Clojure-kod. Antingen behöver du tillfälligt lagra data som du vill komma åt senare, eller så vill du bara testa ett nytt kodsnutt utan att ändra din befintliga kod.

## Så här gör du

Det finns flera olika sätt att skapa en temporär fil i Clojure, beroende på dina specifika behov. Här är ett exempel på hur du kan skapa en temporär fil och skriva data till den:

```Clojure
(require '[clojure.java.io :as io])

(with-open [temp-file (io/file "temp.txt")]
  (io/copy (io/reader "exempel.txt") temp-file))
```

Det första steget är att importera `clojure.java.io` biblioteket med `:as io` som alias. Sedan använder vi `with-open` för att skapa en temporär fil med namnet "temp.txt". Inuti `with-open` blocket, använder vi `io/copy` för att kopiera data från en befintlig fil, här "exempel.txt", till vår temporära fil. Till slut stänger vi temporära filen med `with-open` vilket säkerställer att den blir raderad när blocket är klart.

En annan metod är att använda `io/file` för att direkt skapa en temporär fil utan att behöva använda `with-open`:

```Clojure
(require '[clojure.java.io :as io])

(io/file "temp.txt")
```

Detta kommer också att skapa en temporär fil med namnet "temp.txt". Observera att den här filen inte stängs automatiskt och måste stängas manuellt med `(.delete temp-file)` för att ta bort den.

## Djupdykning

Det finns många andra funktioner och bibliotek som kan hjälpa dig att skapa en temporär fil i Clojure, inklusive `clojure.java.io` och `clojure.data.csv`. Du kan också använda `java.io.File` klassen för att manipulera temporära filer på ett liknande sätt som i Java.

På grund av att temporära filer är osynliga för användaren och endast finns så länge som ditt program körs, är de perfekta för att testa snabbt ut en ny kod utan att behöva skapa permanenta filer eller ändra din befintliga kod.

## Se även

- [Clojure offciell hemsida](https://clojure.org/)
- [Officiell Clojure dokumentation](https://clojure.org/documentation)
- [Clojure subreddit](https://www.reddit.com/r/clojure/)