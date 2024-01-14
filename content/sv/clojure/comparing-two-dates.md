---
title:    "Clojure: Jämföra två datum"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en vanlig uppgift inom programmering, oavsett vilket språk du använder. Det är viktigt att kunna avgöra vilket datum som kommer före eller efter ett annat, för att till exempel sortera data eller filtrera resultat. Med Clojure har du enkla och kraftfulla verktyg för att hantera jämförelser av datum.

## Hur man gör

För att jämföra två datum i Clojure kan du använda funktionen `compare` tillsammans med `#inst` direktiven. Här är ett exempel på hur du kan jämföra två datum:

```Clojure
(let [datum1 #inst "2021-08-31"
      datum2 #inst "2021-09-01"]
    (compare datum1 datum2))
```

Detta kodblock kommer att returnera ett negativt tal om `datum1` kommer före `datum2`, noll om de är lika och ett positivt tal om `datum1` kommer efter `datum2`.

## Djupgående

När du jämför två datum i Clojure bör du vara medveten om att de inte bara innehåller information om årtal, månader och dagar, utan också om klockslag och tidszoner. Om du vill göra en mer exakt jämförelse av två datum måste du också inkludera information om tiden i `#inst` direktiven. Annars kan det hända att två datum som ser likadana ut faktiskt är olika när de inkluderar tiden.

Det är också viktigt att vara medveten om att `compare` funktionen bara jämför datum i UTC-format, vilket kan skapa problem om du har datum i olika tidszoner. För en mer precis jämförelse av datum kan du använda funktionen `clj-time.core/compare` från biblioteket clj-time.

## Se även

- [Clojure.org - Compare](https://clojure.org/api/cheatsheet#compare)
- [Clojuredocs - #inst](https://clojuredocs.org/clojure.core/%23inst)
- [Clj-Time - Compare](https://github.com/clj-time/clj-time#compare)