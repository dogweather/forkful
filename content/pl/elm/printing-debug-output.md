---
title:                "Drukowanie wyników debugowania"
html_title:           "Elm: Drukowanie wyników debugowania"
simple_title:         "Drukowanie wyników debugowania"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Debugowanie – czyli wyświetlanie informacji o tym, co dzieje się w naszej aplikacji w trakcie jej działania – jest często praktykowane przez programistów, ponieważ ułatwia ono zrozumienie i śledzenie kodu. Jest to szczególnie przydatne w przypadku napotkania błędów lub niespodziewanego zachowania aplikacji.

## Jak to zrobić:

```elm
log : String -> a -> a
log prefix value =
    (Debug.log prefix value)
    
main =
    let
        num = 5
    in
        log "Value is:" num
```

**Output:**
```
"Value is: 5"
```

W powyższym przykładzie za pomocą funkcji `log` wyświetlamy wartość zmiennej `num` wraz z podanym prefiksem.

## Głębszy zanurzenie:

Historia tej praktyki sięga lat 70. ubiegłego wieku, kiedy do debugowania wykorzystywano specjalne urządzenia zwane bug box. Dzisiaj debugowanie jest powszechnie stosowane przez programistów jako narzędzie do rozwiązywania problemów i ułatwiania śledzenia kodu. W Elm możemy również wykorzystać funkcję `Debugger` do bardziej szczegółowego debugowania naszej aplikacji.

Alternatywnym sposobem na wyświetlanie debug output jest funkcja `trace`, która jest podobna do `log` z tym wyjątkiem, że zwraca tylko pusty krotkę `()`.

## Zobacz również:

- [Debugging in Elm Documentation](https://elm-lang.org/docs/debugging)
- [Debugging with Elm Remote Devtools](https://laravel-news.com/elm-remote-devtools)
- [Elm Debugger on GitHub](https://github.com/elm-lang/virtual-dom/tree/master/debug)