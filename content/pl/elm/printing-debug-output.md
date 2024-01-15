---
title:                "Wyświetlanie wyników debugowania"
html_title:           "Elm: Wyświetlanie wyników debugowania"
simple_title:         "Wyświetlanie wyników debugowania"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem podczas pisania kodu trzeba dokonać pewnych zmian lub zrozumieć dlaczego program działa w taki, a nie inny sposób. W takich sytuacjach pomocne może być wypisywanie wyników debugowania.  

## Jak to zrobić

Aby wypisać wyniki debugowania w Elm, możemy skorzystać z funkcji `Debug.log`. Ta metoda przyjmuje dwa parametry: string, który będzie mówił nam co chcemy wypisać oraz wartość, którą chcemy zobaczyć. 

```Elm 
Debug.log "Debugowanie" "Wartość zmiennej to:"
```

W powyższym przykładzie, po wykonaniu kodu, w konsoli zobaczymy napis "Debugowanie: Wartość zmiennej to:". Możemy także wyświetlać wartości zmiennych, np. `model`:

```Elm
Debug.log "Model" model
```

W przypadku bardziej skomplikowanych wartości, możemy wykorzystać funkcję `toString`, aby przekonwertować je na string przed wypisaniem.

## Deep Dive

Podczas debugowania, dobrze jest uważać, aby nie zostawić funkcji `Debug.log` po zakończeniu pracy nad kodem. Wypisywanie informacji debugowania może znacznie spowolnić działanie programu. Aby temu zapobiec, warto skorzystać z flagi `--optimize`, która będzie pomijać funkcje `Debug.log` w kodzie produkcyjnym, czyli tym wykonywanym po zbudowaniu aplikacji.

## Zobacz także

- [Oficjalna dokumentacja Elm - Debug](https://guide.elm-lang.org/debugging/debug.html)
- [Debugowanie w Elm - blog post](https://medium.com/@pavelbogomolenko/using-debug-in-elm-d5e0fe254012)