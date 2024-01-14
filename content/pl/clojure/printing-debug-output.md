---
title:    "Clojure: Wydrukowanie danych do debugowania"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawialiście się kiedyś dlaczego warto wyświetlać debugowe wyjście podczas pisania kodu w Clojure? Czy wiecie, że może to pomóc w szybkim i skutecznym debugowaniu błędów? W tym artykule dowiecie się dlaczego warto tego używać i jak to zrobić.

## Jak to zrobić

Aby wyświetlić debugowe wyjście w Clojure, możemy skorzystać z funkcji `println`, która wypisze nam wartości w konsoli. Spójrzmy na poniższy przykład:

```Clojure
(defn sum [a b]
  (println "Dodaję" a "do" b)
  (+ a b))
  
(println "Wynik:" (sum 5 7))

Wynik: 12
```

Funkcja `println` może przyjmować dowolną liczbę argumentów i przekonwertuje je na tekst, dzięki czemu możemy wyświetlać zmienne, wyniki działań czy też komunikaty związane z aktualnie wykonywanym kodem. To może bardzo ułatwić nam zrozumienie wykonywanego kodu i pomóc w znalezieniu ewentualnych błędów.

## Deep Dive

Istnieje również możliwość użycia funkcji `prn`, która działa podobnie jak `println`, ale dodatkowo przekonwertuje wyjściowe wartości do ich reprezentacji tekstowej w języku Clojure. Dzięki temu możemy uniknąć niechcianych konwersji, które mogą wpłynąć na wygląd wyjścia.

Warto również wspomnieć o funkcji `println-str`, która wypisze wartości do zmiennej typu `String` zamiast na konsolę. Może to być przydatne w przypadku, gdy chcemy przekazać wyjście do innego miejsca w naszym kodzie.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o debugowaniu w Clojure, polecam zapoznać się z poniższymi artykułami:

- [Clojure dla początkujących: Debugowanie](https://jakubniewczas.pl/clojure-dla-poczatkujacych-debugowanie/)
- [Debugowanie w Clojure z pomocą biblioteki Clover](https://blog.softwaremill.com/debugging-clojure-code-with-the-help-of-clover-library-59fbd049952a)
- [Podstawy debugowania w Clojure](https://jacekszubert.tech/2018/04/02/debugging-in-clojure-basic/)

Dzięki wyświetlaniu debugowego wyjścia nasz kod będzie łatwiejszy w debugowaniu, co przyczyni się do bardziej efektywnej i przyjemniejszej pracy. Pamiętajmy jednak, aby nie zostawiać tych instrukcji w kodzie produkcyjnym, gdyż może to wpłynąć na wydajność aplikacji.

## Zobacz również

Nie zapomnij zajrzeć na naszą stronę [ClojurePL](https://clojure.pl/) oraz do naszej społeczności na Slacku, gdzie możesz znaleźć więcej ciekawych artykułów i dyskusji na temat programowania w Clojure. Do zobaczenia!