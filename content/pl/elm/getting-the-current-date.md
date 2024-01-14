---
title:    "Elm: Pobieranie bieżącej daty"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się niezwykle popularne, a coraz więcej osób zaczyna się interesować w nim. Jednym z języków programowania, który zyskuje na popularności jest Elm. Ma on wiele zalet, w tym przejrzystą składnię i silną statyczną typizację. Jedną z bardzo przydatnych funkcji, która może zainteresować początkujących programistów, jest możliwość uzyskiwania aktualnej daty w języku Elm. W tym artykule dowiesz się dlaczego warto znać ten sposób i jak go użyć.

## Jak To Zrobić

Aby uzyskać aktualną datę w Elm, należy wykorzystać funkcję `Time.now`, która zwraca wartość typu `Time`, reprezentującą bieżący czas i datę. Aby ją wyświetlić w konsoli, możemy użyć funkcji `Time.toString` i przekazać jej pierwszy argument jako wynik funkcji `Time.now`. Poniżej znajduje się przykładowy kod:

```Elm
import Time exposing (..)

main : Program ()
main =
  let
    currentTime = Time.now
  in
    Time.toString currentTime

```

W powyższym kodzie `main` jest główną funkcją programu, która przyjmuje typ `Program ()`, a następnie wywołuje funkcje `now`i `toString` z modułu `Time`. Wykonując powyższy kod, otrzymamy na wyjściu aktualną datę i godzinę w postaci tekstowej.

```
2019-05-09T21:49:48.3Z
```

Możemy również wyświetlić tylko konkretną część daty, np. rok lub dzień, stosując odpowiednie funkcje z modułu `Time`.

## Głębsza Analiza

W języku Elm, istnieje kilka funkcji, które mogą być przydatne podczas pracy z datami. Najważniejszą z nich jest `Time.now`, ale można również skorzystać z `Time.fromTimeStamp`, `Time.millisecond`, `Time.second`, `Time.minute` i `Time.hour`, które pozwalają na konwertowanie daty na różne jednostki czasu. Można również wykorzystać funkcję `Time.since`, aby obliczyć czas od danego punktu w przeszłości lub `Time.inDays` do konwersji czasu w dni.

## Zobacz również

- [Oficjalna dokumentacja Elm](https://elm-lang.org/docs)
- [Przewodnik dla początkujących w Elm](https://guide.elm-lang.org/)
- [Repl.it - narzędzie online do nauki języka Elm](https://repl.it/languages/elm)