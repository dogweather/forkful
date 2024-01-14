---
title:                "Clojure: Läsning av kommandoradsargument"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument är ett vanligt scenario när man bygger program som behöver ta emot användarinput. I denna bloggpost kommer vi att utforska hur man kan läsa in och hantera kommandoradsargument i Clojure.

## Så här gör du

För att läsa in kommandoradsargument i Clojure kan du använda funktionen `*command-line-args*`. Denna funktion returnerar en vektor av strängar som representerar de argument som skickats till programmet. Låt oss titta på ett exempel:

```Clojure
(defn print-args []
  (println "Antal argument:" (count *command-line-args*))
  (println "Argument:" *command-line-args*))
```
Om vi kör denna funktion med argumenten "hej" och "världen" i terminalen:

```
clojure -m program hej världen
```

så kommer vi att få följande output:

```
Antal argument: 2
Argument: ["hej" "världen"]
```

Detta visar hur funktionen `*command-line-args*` returnerar en vektor med de inmatade argumenten. Nu kan du använda denna vektor för att dynamiskt behandla och använda argumenten i ditt program.

## Uppforska djupare

Det finns många olika sätt att hantera kommandoradsargument i Clojure, och det är viktigt att utforska de olika möjligheterna för att hitta den bästa lösningen för ditt projekt. Du kan till exempel använda funktionen`clojure.string/split` för att dela upp argumenten eller använda bibliotek som `clj-argparser` för mer avancerad argumenthantering. Utforska och experimentera för att hitta den lösning som passar dig bäst.

## Se även

- [Clojure API-dokumentation för *command-line-args*](https://clojuredocs.org/clojure.core/*command-line-args*)
- [Clj-argparser](https://github.com/zcaudate/clj-argparser)
- [Officiell Clojure Tutorial](https://clojure.org/guides/getting_started)

**Detta var allt för denna bloggpost! Hoppas det har varit användbart för dig när du arbetar med kommandoradsargument i Clojure. Lycka till!**