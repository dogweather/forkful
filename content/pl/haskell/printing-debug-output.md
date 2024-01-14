---
title:                "Haskell: Wypisywanie wyjścia debugowania"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas programowania napotykamy błędy i problemy, które są trudne do zdiagnozowania tylko na podstawie kodu. W takich przypadkach pomocne może być wypisywanie informacji o stanie naszej aplikacji za pomocą kodu debugowania. W artykule tym dowiesz się, dlaczego warto drukować informacje debugowania w twoim kodzie Haskell.

## Jak to zrobić

W celu wypisywania informacji debugowania w kodzie Haskell, możemy używać funkcji `Debug.Trace.trace` z modułu `Debug.Trace`. Poniżej znajduje się przykładowy kod wraz z komentarzami, który pokaże ci, jak to zrobić:

```Haskell
import Debug.Trace -- importujemy moduł Debug.Trace, aby użyć funkcji trace

main :: IO ()
main = do
  -- tworzymy przykładową listę liczb
  let numbers = [1, 2, 3, 4, 5]

  -- iterujemy przez listę i wypisujemy informację debugowania przy każdej iteracji
  -- funkcja `trace` pobiera dwa argumenty - wiadomość do wypisania i wartość, którą chcemy wypisać
  -- w tym przypadku wartością jest aktualny element listy
  forM_ numbers $ \n -> do
    trace ("Aktualnie przetwarzany element to: " ++ show n) n

```

Po uruchomieniu tego kodu, powinieneś zobaczyć informacje debugowania przy każdej iteracji w konsoli:

```
Aktualnie przetwarzany element to: 1
Aktualnie przetwarzany element to: 2
Aktualnie przetwarzany element to: 3
Aktualnie przetwarzany element to: 4
Aktualnie przetwarzany element to: 5
```

## Deep Dive

Warto zwrócić uwagę, że funkcja `Debug.Trace.trace` jest przeznaczona tylko do celów debugowania i nie powinna być używana w kodzie produkcyjnym. Ponadto, jej użycie może mieć negatywny wpływ na wydajność naszego programu. Dlatego zalecane jest usuwanie wszystkich wywołań tej funkcji po zakończeniu procesu debugowania.

## Zobacz również

- Dokumentacja funkcji `Debug.Trace.trace`: http://hackage.haskell.org/package/base-4.14.0.0/docs/Debug-Trace.html#g:1
- Przykładowe zadania związane z debugowaniem w Haskell: https://adventofcode.com/
- Blog o programowaniu w Haskell (w języku polskim): https://hask.pl/