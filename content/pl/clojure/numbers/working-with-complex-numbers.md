---
date: 2024-01-26 04:39:13.454494-07:00
description: "Liczby zespolone rozszerzaj\u0105 liczby rzeczywiste o dodatkow\u0105\
  \ cz\u0119\u015B\u0107, jednostk\u0119 urojon\u0105 'i'. Programi\u015Bci u\u017C\
  ywaj\u0105 ich w r\xF3\u017Cnych dziedzinach, w tym w\u2026"
lastmod: '2024-02-25T18:49:33.413334-07:00'
model: gpt-4-0125-preview
summary: "Liczby zespolone rozszerzaj\u0105 liczby rzeczywiste o dodatkow\u0105 cz\u0119\
  \u015B\u0107, jednostk\u0119 urojon\u0105 'i'. Programi\u015Bci u\u017Cywaj\u0105\
  \ ich w r\xF3\u017Cnych dziedzinach, w tym w\u2026"
title: Praca z liczbami zespolonymi
---

{{< edit_this_page >}}

## Co i Dlaczego?
Liczby zespolone rozszerzają liczby rzeczywiste o dodatkową część, jednostkę urojoną 'i'. Programiści używają ich w różnych dziedzinach, w tym w przetwarzaniu sygnałów, teorii elektromagnetyzmu i fraktalach, gdzie obliczenia związane z pierwiastkiem kwadratowym z liczby ujemnej są rutyną.

## Jak to zrobić:
Clojure oferuje wbudowane wsparcie dla liczb zespolonych poprzez klasę narzędziową `clojure.lang.Numbers`. Użyj `complex`, aby tworzyć liczby zespolone i wykonywać działania arytmetyczne.

```clojure
;; Tworzenie liczb zespolonych
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Dodawanie
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Odejmowanie
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Mnożenie
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Dzielenie
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Sprzężenie
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Dogłębne rozpatrzenie
Liczby zespolone zostały sformalizowane przez matematyków takich jak Gauss i Euler w XVIII wieku. Chociaż początkowo spotkały się ze sceptycyzmem, od tamtej pory stały się kluczowe w nowoczesnej nauce i inżynierii. Clojure nie ma natywnego typu liczby zespolonej jak niektóre języki (np. Python), ale dołączona interoperacyjność z Javą może obsługiwać niezbędne operacje za pośrednictwem klasy `clojure.lang.Numbers`.

`java.lang.Complex` Javy to solidna alternatywa, oferująca więcej funkcji i potencjalnych optymalizacji. Interoperacyjność hosta Clojure ułatwia pracę z bibliotekami Javy.

W skrócie, arytmetyka liczb zespolonych obejmuje dodawanie i mnożenie części rzeczywistych i urojonych, z kluczową zasadą, że `i^2 = -1`. Dzielenie liczb zespolonych może być bardziej zawiłe, zwykle wymaga użycia sprzężenia, aby uniknąć dzielenia przez liczby zespolone.

## Zobacz również
- ClojureDocs, dla szybkiego odniesienia: https://clojuredocs.org/
- API Javy dla `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- Strona Wikipedii o liczbach zespolonych dla matematycznie ciekawskich: https://en.wikipedia.org/wiki/Complex_number
