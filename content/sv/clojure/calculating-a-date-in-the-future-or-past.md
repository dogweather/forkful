---
title:    "Clojure: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller förflutet är en mycket användbar funktion i många programmeringsprojekt. Det kan hjälpa dig att planera händelser och göra förutsägelser baserat på historiska data.

## Hur man gör
För att beräkna ett datum i framtiden eller förflutet i Clojure, kan du använda funktionen "clj-time". Först måste du importera biblioteket genom att lägga till följande kod i din fil:

```Clojure
(ns min-projekt.core
  (:require [clj-time.core :as t]))
```

När du har gjort det kan du använda funktionen "t/plus" för att beräkna ett datum i framtiden eller förflutet. Den tar två argument - det datum som du vill utgå ifrån och en "period" som anger hur många dagar, veckor, månader eller år du vill lägga till eller subtrahera från datumet. Till exempel om du vill beräkna dagen som är 10 dagar efter idag kan du skriva följande kod:

```Clojure
(t/plus (t/today) (t/days 10))
```

Kom ihåg att funktionen "t/plus" returnerar ett "Date"-objekt, så för att få ut datumet i ett visst format kan du använda funktionen "t/formatter". Till exempel om du vill få ut datumet i formatet "yyyy-MM-dd" kan du skriva följande kod:

```Clojure
(t/formatter "yyyy-MM-dd" (t/plus (t/today) (t/days 10)))
```
Detta kommer att returnera ett resultat som ser ut så här: "2020-05-20".

## Djupdykning
För att göra mer avancerade beräkningar finns det flera andra funktioner och argument som du kan använda med "t/plus". Till exempel kan du specificera vilken veckodag du vill ha som resultat genom att använda argumentet "tobe" och ett nummer mellan 1-7, där 1 motsvarar måndag och 7 motsvarar söndag. Om du till exempel vill få ut nästa torsdag från dagens datum kan du skriva följande kod:

```Clojure
(t/plus (t/today) (t/weeks 1) :tobe 4)
```

Detta kommer att returnera ett datum som motsvarar den närmaste torsdagen från dagens datum.

## Se även
- [Clj-time dokumentation](https://github.com/clj-time/clj-time)
- [Officiell Clojure hemsida](https://clojure.org/)
- [Clojuredocs - en community driven dokumentation för Clojure](https://clojuredocs.org/)