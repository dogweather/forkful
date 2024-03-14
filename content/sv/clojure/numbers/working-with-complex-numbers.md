---
date: 2024-01-26 04:38:46.696347-07:00
description: "Komplexa tal utvidgar de reella talen med en ytterligare del, den imagin\xE4\
  ra enheten 'i'. Programmerare anv\xE4nder dem inom olika omr\xE5den, inklusive\u2026"
lastmod: '2024-03-13T22:44:37.519450-06:00'
model: gpt-4-0125-preview
summary: "Komplexa tal utvidgar de reella talen med en ytterligare del, den imagin\xE4\
  ra enheten 'i'. Programmerare anv\xE4nder dem inom olika omr\xE5den, inklusive\u2026"
title: Att arbeta med komplexa tal
---

{{< edit_this_page >}}

## Vad & Varför?
Komplexa tal utvidgar de reella talen med en ytterligare del, den imaginära enheten 'i'. Programmerare använder dem inom olika områden, inklusive signalbehandling, elektromagnetisk teori och fraktaler, där beräkningar som involverar kvadratroten av ett negativt tal är rutin.

## Hur man gör:
Clojure erbjuder inbyggt stöd för komplexa tal genom verktygsklassen `clojure.lang.Numbers`. Använd `complex` för att skapa komplexa tal och utföra aritmetik.

```clojure
;; Skapa komplexa tal
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Addition
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Subtraktion
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Multiplikation
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Division
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Konjugat
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Fördjupning
Komplexa tal formaliserades av matematiker som Gauss och Euler under 1700-talet. Även om de initialt möttes med skepticism har de sedan dess blivit avgörande i modern vetenskap och teknik. Clojure har inte en inbyggd typ för komplexa tal som vissa andra språk (t.ex. Python), men den inkluderade Java-interoperabiliteten kan hantera nödvändiga operationer via klassen `clojure.lang.Numbers`.

Javas `java.lang.Complex` är ett robust alternativ som erbjuder fler funktioner och potentiella optimeringar. Clojures värdinteroperabilitet gör det enkelt att arbeta med Java-bibliotek.

Under huven innebär aritmetik med komplexa tal att addera och multiplicera de reella och imaginära delarna, med huvudregeln att `i^2 = -1`. Division av komplexa tal kan vara mer komplicerat, typiskt krävande användning av konjugatet för att undvika division med komplexa tal.

## Se även
- ClojureDocs, för snabbreferens: https://clojuredocs.org/
- Java API för `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- Wikipediasida om komplexa tal för den matematiskt nyfikne: https://sv.wikipedia.org/wiki/Komplext_tal
