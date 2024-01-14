---
title:    "Elm: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego
W dzisiejszych czasach technologia zmienia się bardzo szybko, a programiści muszą być w stanie dostosowywać się do zmieniających się wymagań. Jednym z podstawowych zadań w programowaniu jest obliczanie dat w przyszłości lub przeszłości. W tym artykule dowiecie się, dlaczego obliczanie dat może być przydatne i jak wykonać to zadanie w języku Elm.

## Jak to zrobić
Aby obliczyć datę w przyszłości lub przeszłości w języku Elm, musimy użyć funkcji Date.add. Przykładowy kod wyglądałby następująco:
```
Elm

import Date exposing (add, millisecond, hour)
import Time exposing (millisecond)
 
futureDate = Date.now |> add (5 * hour)
previousDate = Date.now |> add (-10 * millisecond)
 
```

W powyższym kodzie importujemy odpowiednie moduły, a następnie wykorzystujemy funkcję add, która przyjmuje dwa parametry - datę i okres czasu, o jaki chcemy przesunąć tę datę. W przykładzie powyżej dodajemy 5 godzin do aktualnej daty i odejmujemy 10 milisekund.

Po wykonaniu tego zadania możemy wyświetlić uzyskane wyniki przy użyciu funkcji toString, która konwertuje datę na czytelny dla nas format.

## Głębszy dive
Obliczanie dat w przyszłości lub przeszłości może być przydatne w wielu różnych scenariuszach. Na przykład, przy tworzeniu aplikacji przypominających o nadchodzących wydarzeniach lub przy planowaniu zadania do wykonania w przyszłości. W przypadku gdy musimy wyświetlić użytkownikowi datę, która jest ustawiona w przyszłości lub przeszłości, możemy również wykorzystać funkcję Date.after, która sprawdza, czy dana data znajduje się po drugiej, podanej w parametrze.

Mamy również dostęp do innych funkcji związanych z datami, takich jak Date.fromTime, które konwertuje podany czas (w milisekundach) na datę, lub Date.toUnix, która zwraca liczbę sekund od 1 stycznia 1970 roku do podanej daty.

## Zobacz również
- Oficjalna dokumentacja Elm dotycząca obliczania dat: https://guide.elm-lang.org/core_language.html#dates-and-time
- Przykładowy projekt obliczający daty w przyszłości i przeszłości w języku Elm: https://github.com/marella/basic-date-calculator-elm
- Poradnik dotyczący pracy z datami i czasem w Elm: https://medium.com/@grysz/working-with-dates-and-time-in-elm-14f813900e5e