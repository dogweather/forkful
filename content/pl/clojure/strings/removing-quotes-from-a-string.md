---
date: 2024-01-26 03:40:30.716713-07:00
description: "Jak to zrobi\u0107: W Clojure \u0142a\u0144cuchy znak\xF3w s\u0105 niezmienne,\
  \ wi\u0119c kiedy m\xF3wimy o \"usuwanie cudzys\u0142ow\xF3w\", naprawd\u0119 m\xF3\
  wimy o tworzeniu nowego \u0142a\u0144cucha znak\xF3w bez\u2026"
lastmod: '2024-03-13T22:44:34.981974-06:00'
model: gpt-4-0125-preview
summary: "W Clojure \u0142a\u0144cuchy znak\xF3w s\u0105 niezmienne, wi\u0119c kiedy\
  \ m\xF3wimy o \"usuwanie cudzys\u0142ow\xF3w\", naprawd\u0119 m\xF3wimy o tworzeniu\
  \ nowego \u0142a\u0144cucha znak\xF3w bez cudzys\u0142ow\xF3w."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
W Clojure łańcuchy znaków są niezmienne, więc kiedy mówimy o "usuwanie cudzysłowów", naprawdę mówimy o tworzeniu nowego łańcucha znaków bez cudzysłowów. Oto sposób użycia `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; Pozbądźmy się tych podwójnych cudzysłowów
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; I wyeliminujmy pojedyncze cudzysłowy
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Przykładowe użycie:
(remove-double-quotes "\"Cześć, Świecie!\"") ; => "Cześć, Świecie!"
(remove-single-quotes "'Cześć, Świecie!'")   ; => "Cześć, Świecie!"
```
Chcesz poradzić sobie zarówno z pojedynczymi, jak i podwójnymi cudzysłowami za jednym zamachem? Spójrz na to:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Przykładowe użycie:
(remove-quotes "\"Cześć, 'Clojure' Świecie!\"") ; => "Cześć, Clojure Świecie!"
```

## Głębsze zanurzenie
W dawnych czasach, gdy dane były bardziej nieporządne niż pokój dziecięcy, cudzysłowy w łańcuchach znaków były normą do oznaczania tekstu. Ale wraz z ewolucją informatyki, cudzysłowy stały się czymś więcej niż tylko delimiterami tekstu – przejęły role syntaktyczne w językach programowania.

Clojure, z jego dziedzictwem Lispu, nie używa cudzysłowów w ten sam sposób co niektóre inne języki. Z pewnością są używane do oznaczania łańcuchów znaków, ale mają też specjalną rolę w tworzeniu literałów. Niemniej jednak, usuwanie cudzysłowów z łańcuchów znaków pozostaje ponadczasowym zadaniem.

Dlaczego nie wystarczy po prostu obciąć końców łańcucha znaków? Cóż, to zakłada, że twoje cudzysłowy zawsze przytulają początek i koniec twojego łańcucha niczym para zbyt czułych dziadków. Rzeczywiste dane są bardziej nieuporządkowane. Tu na scenę wkraczają wyrażenia regularne (regex), które pozwalają ci celować w te cudzysłowy, niezależnie od tego, gdzie się ukrywają.

Alternatywy? Jasne, możesz zaszaleć z `subs`, `trim`, `triml`, `trimr`, czy nawet z transduktorami, jeśli chcesz popisać się umiejętnościami. Ale `replace` z regexem to jak przynieść miecz świetlny na walkę na noże – idzie prosto do celu.

## Zobacz również
Jeśli twój mózg pragnie więcej dobroci związanej z manipulacją łańcuchami znaków w Clojure, te okruchy informacji mogą pomóc:

- ClojureDocs na temat `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Wyrażenia regularne w Clojure: https://clojure.org/guides/learn/syntax#_regex
- Interoperacyjność z Javą dla obsługi łańcuchów znaków (w końcu Clojure działa na JVM): https://clojure.org/reference/java_interop#_working_with_strings

Nie zatrzymuj się tylko na usuwaniu cudzysłowów. W Clojure-landii czeka do odkrycia cały świat czarów ze łańcuchami znaków.
