---
title:                "Usuwanie cudzysłowów z ciągu znaków"
aliases:
- pl/clojure/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:30.716713-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie cudzysłowów ze łańcucha znaków oznacza pozbycie się tych uporczywych znaków podwójnych lub pojedynczych cudzysłowów, które otaczają twój tekst. Programiści robią to, aby oczyścić dane, zapewnić jednolitość lub przygotować łańcuchy znaków do przetwarzania, gdzie cudzysłowy są niepożądane lub mogą powodować błędy.

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
