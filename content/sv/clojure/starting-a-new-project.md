---
title:                "Att börja ett nytt projekt"
html_title:           "Clojure: Att börja ett nytt projekt"
simple_title:         "Att börja ett nytt projekt"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför 
När det kommer till programmeringsspråk, finns det många alternativ att välja mellan. Så varför ska du välja Clojure för att starta ett nytt projekt? Clojure, som är ett funktionellt programeringsspråk som körs på Java Virtuell Maskin (JVM), har många fördelar som gör det till ett attraktivt val. Det är enkelt att lära sig, har ett minimalistiskt syntax och möjliggör enkel parallell programmering.

## Hur man gör
För att börja ett nytt Clojure-projekt, börja med att installera Leiningen, som är ett byggverktyg för Clojure. Följ sedan nedanstående steg:

1. Skapa en katalog för ditt projekt.
2. Öppna din terminal och navigera till den skapade katalogen.
3. Skapa en ny fil med filändelsen ```project.clj```. Detta bestämmer inställningarna för ditt projekt.
4. I filen, skriv följande kod som specificerar projektets grundinställningar:
   ```
   (defproject min-nya-projekt "0.1.0"
     :description "Ett enkelt Clojure-projekt"
     :dependencies [[org.clojure/clojure "1.10.1"]])
   ```
5. Spara filen och stäng den.
6. Öppna en ny fil med filändelsen ```core.clj```. Detta kommer att vara huvudfilen för ditt projekt.
7. Skriv den grundläggande koden för ditt projekt, till exempel en enkel "Hello world!"-applikation:
   ```
   (ns min-nya-projekt.core
     (:gen-class))
   (defn -main []
     (println "Hello world!"))
   ```
8. Spara filen och stäng den.
9. I din terminal, kör följande kommando för att bygga ditt projekt:
   ```
   lein compile
   ```
10. När byggprocessen är klar, kör följande kommando för att köra ditt projekt:
    ```
    lein run
    ```
11. Om allt är korrekt, bör du nu se "Hello world!"-meddelandet på din terminal.

## Djupdykning
När du väl är bekant med grundläggande koncept i Clojure, finns det många användbara bibliotek som du kan använda för att utöka funktionaliteten i ditt projekt. Till exempel, för webbutveckling kan du använda biblioteket Ring för att skapa en webbserver eller biblioteket Compojure för att skapa vägar för din webbapplikation.

En annan fördel med Clojure är att det är lätt att integrera med Java-kod. Detta gör det möjligt att använda befintliga Java-bibliotek och utnyttja dess funktionalitet i ditt Clojure-projekt.

En annan viktig aspekt vid att starta ett nytt projekt är att använda ett versionshanteringssystem som Git. Detta gör det möjligt för dig att hantera kodändringar och samarbeta med andra utvecklare. För Clojure-projekt är det också fördelaktigt att använda verktyget Leiningen för att hantera beroenden och bygga projektet.

## Se även
- [Leiningen](https://leiningen.org/)
- [Clojure dokumentation](https://clojure.org/guides/getting_started)
- [Ring](https://github.com/ring-clojure/ring)
- [Compojure](https://github.com/weavejester/compojure)