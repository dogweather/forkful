---
title:                "Haskell: Wydrukowanie wyników debugowania"
simple_title:         "Wydrukowanie wyników debugowania"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyświetlanie informacji debugujących jest ważnym narzędziem podczas tworzenia programów w Haskellu. Pozwala na łatwe śledzenie i zrozumienie działania programu oraz diagnozowanie ewentualnych błędów.

## Jak to zrobić

Aby wyświetlać informacje debugujące w Haskellu, wystarczy użyć funkcji `print`. Przykładowy kod wyglądałby następująco:

```Haskell
myList = [1, 2, 3]
print myList
```

Otrzymalibyśmy następujący output:

```
[1,2,3]
```

Można także użyć funkcji `putStrLn` w celu wyświetlenia tekstu lub wartości innych typów danych. Przykład:

```Haskell
print "Hello World"
print 123
```

Output:

```
Hello World
123
```

## Wgłąb tematu

Istnieje wiele metod wyświetlania informacji debugujących, w tym także biblioteki specjalnie zaprojektowane do tego celu, np. `Debug.Trace`. Ta biblioteka pozwala na dodawanie śledzonych punktów w kodzie oraz wyświetlanie wartości zmiennych w tych punktach.

Używanie informacji debugujących jest szczególnie przydatne w przypadku skomplikowanych funkcji lub kodu, gdzie trudno jest zrozumieć jego działanie tylko na podstawie samego kodu. Dzięki wyświetlaniu informacji debugujących można łatwiej zlokalizować błędy i szybciej je naprawić.

## Zobacz także

- [Dokumentacja biblioteki Debug.Trace](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)
- [Tutorial nt. debugowania w Haskellu](https://serokell.io/blog/haskell-debugging-tips)
- [Poradnik nt. użytkowania funkcji `print` i `putStrLn`](https://www.tutorialspoint.com/haskell/haskell_input_output.htm)