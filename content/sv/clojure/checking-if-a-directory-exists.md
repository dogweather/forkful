---
title:                "Clojure: Kontrollera om en mapp existerar"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av Clojure-programmering eftersom det ger dig möjligheten att utföra specifika åtgärder baserat på mappens befintlighet. Detta är särskilt användbart i situationer där du behöver interagera med filsystemet.

## Hur man gör det

För att kontrollera om en mapp existerar i Clojure, kan du använda funktionen ```clojure.java.io/file```. Med denna funktion kan du skapa en instans av en mapp och sedan använda ```.exists``` för att kontrollera om den faktiskt existerar.

```Clojure
(def directory (clojure.java.io/file "sökväg/till/mappen"))

(directory.exists)

=> true
```

Om mappen inte existerar kommer funktionen ```directory.exists``` att returnera ```false```.

## Djupdykning

När du använder ```clojure.java.io/file``` för att skapa en instans av en mapp, kan du faktiskt passa in flera argument. Dessa argument kan inkludera en referens till en annan mapp, en URI eller en URL. Du kan också ange flera argument som leder till en specifik mapp eller fil.

En annan användbar funktion är ```directory?``` som kan användas för att kontrollera om en viss fil är en mapp eller inte.

```Clojure
(directory?)

=> true
```

En annan intressant funktion är ```resolve-path``` som kan användas för att få den fullständiga sökvägen till en mapp eller fil. Detta kan vara särskilt användbart när du behöver skapa en mapp eller fil på en specifik plats.

## Se även

- [Clojure Dokumentation](https://clojure.org/guides/learn/syntax)
- [Clojure Verktyg för Fil- och Mappinteraktion](https://clojure.github.io/java.io/)