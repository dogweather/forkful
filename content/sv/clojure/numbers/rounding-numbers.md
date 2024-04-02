---
date: 2024-01-26 03:43:30.285703-07:00
description: "Avrundning av tal handlar om att justera ett tal till n\xE4rmaste hela\
  \ tal eller till en viss decimalprecision. Vi avrundar tal f\xF6r att f\xF6renkla\
  \ dem f\xF6r\u2026"
lastmod: '2024-03-13T22:44:37.520388-06:00'
model: gpt-4-0125-preview
summary: "Avrundning av tal handlar om att justera ett tal till n\xE4rmaste hela tal\
  \ eller till en viss decimalprecision. Vi avrundar tal f\xF6r att f\xF6renkla dem\
  \ f\xF6r\u2026"
title: Avrundning av tal
weight: 13
---

## Vad och Varför?
Avrundning av tal handlar om att justera ett tal till närmaste hela tal eller till en viss decimalprecision. Vi avrundar tal för att förenkla dem för mänsklig läsbarhet, minska beräkningsbelastningen eller uppfylla specifika numeriska krav.

## Hur man gör:
I Clojure använder vi främst `Math/round`, `Math/floor` och `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

För specifika decimalplatser multiplicerar, avrundar och dividerar vi:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Djupdykning
Innan programmeringsspråk var avrundning en manuell process, tänk abakus eller papper. I programmering är det avgörande för representation av tal på grund av begränsningar i precisionen hos flyttal.

Alternativ för avrundning inkluderar användning av klassen `BigDecimal` för precisionskontroll eller bibliotek som `clojure.math.numeric-tower` för avancerade matematikfunktioner. Clojures `Math/round` förlitar sig på Javas `Math.round`, `Math/floor` och `Math/ceil` funktioner, vilket innebär att det ärver samma nyanser av float och double.

När det gäller implementering, när du avrundar i Clojure, kom ihåg att det automatiskt använder dubbel precision när det hanterar decimaler. Var försiktig med avrundningsfel!

## Se även
- Clojure Math API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java Math API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Att förstå precisionen hos flyttal: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
