---
title:    "Clojure: Att börja ett nytt projekt"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Varför

Att starta ett nytt projekt i Clojure kan vara en spännande utmaning för programmerare. Med sin funktionella programmeringstil och möjlighet till interaktiv utveckling, är Clojure ett kraftfullt verktyg som kan hjälpa till att lösa komplexa problem på ett effektivt sätt.

# Så här gör du

För att komma igång med ett nytt projekt i Clojure, behöver du först och främst installera Clojure på din dator. Sedan kan du använda ett byggverktyg som Leiningen för att skapa ett nytt projekt. Här är en exempelkod som visar hur du kan skapa ett nytt projekt med namnet "hello-world":

```Clojure
lein new app hello-world
```

När projektet är skapat kan du navigera till dess mapp i din terminal och köra följande kommando för att starta en interaktiv REPL-miljö:

```Clojure
lein repl
```

Nu kan du börja skriva och testa din kod direkt i REPL-miljön. För att avsluta REPL-miljön kan du använda kommandot "exit" eller trycka på "Ctrl + D" på tangentbordet.

Ett enkelt program för att skriva ut "Hello, world!" i Clojure kan se ut så här:

```Clojure
(defn hello-world [] 
    (println "Hello, world!"))
```

När du är färdig med ditt projekt kan du bygga det genom att köra följande kommando:

```Clojure
lein uberjar
```

Detta kommer att skapa en körbar JAR-fil som du kan använda för att köra ditt program utanför REPL-miljön.

# Djupdykning

När du startar ett nytt projekt i Clojure, kan det vara bra att ha en tydlig struktur för ditt projekt. En vanlig hierarki för ett Clojure-projekt ser ut så här:

- src (för programkod)
- test (för testfall)
- resources (för konfigurationsfiler eller andra resurser som behövs av din kod)
- target (där de kompilerade filerna hamnar)
- project.clj (projektets konfigurationsfil)

Det är också viktigt att planera och tänka på vilka dependencies (beroenden) som ditt projekt behöver. Du kan lägga till dependencies i din projekt.clj-fil och Leiningen kommer att hämta dem åt dig när du bygger ditt projekt.

# Se även

- [Clojure.org](https://clojure.org/)
- [Clojure Dokumentation](https://clojure.org/documentation)
- [Leiningen](https://leiningen.org/)