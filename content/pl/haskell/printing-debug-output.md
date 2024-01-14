---
title:    "Haskell: Wydrukowanie wyników debugowania"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Debugowanie jest ważnym krokiem w procesie programowania. Dzięki wyświetlaniu komunikatów debugowych można łatwiej zrozumieć działanie programu i naprawić ewentualne błędy. W tym artykule dowiesz się, dlaczego warto wyświetlać komunikaty debugowe podczas pisania kodu w języku Haskell.

## Jak to zrobić?

W języku Haskell najczęściej do wyświetlania komunikatów debugowych używa się funkcji `print`. Przykładowe użycie tej funkcji wygląda następująco:

```Haskell
main :: IO ()
main = do
  let x = 10
  print x
```

W tym kodzie zmienna `x` zostanie wyświetlona w konsoli, co umożliwi nam sprawdzenie jej wartości w trakcie działania programu. Można także wyświetlić więcej niż jedną zmienną, po prostu przekazując je jako kolejne argumenty funkcji `print`:

```Haskell
main :: IO ()
main = do
  let x = 10
      y = "Hello"
  print x y
```

Wynikiem będzie wyświetlenie wartości `x` i `y` w kolejności, w jakiej zostały przekazane do funkcji `print`.

## Dogłębne wgląd

Wyświetlanie komunikatów debugowych jest szczególnie przydatne w przypadku bardziej skomplikowanych programów, gdzie trudno od razu rozpoznać źródło błędu. Można również skorzystać z funkcji `show` do wyświetlania bardziej skomplikowanych typów danych, takich jak listy czy krotki. Przykładowo:

```Haskell
main :: IO ()
main = do
  let list = [1,2,3]
  print ("Lista: " ++ show list)
```

Wynikiem będzie wyświetlenie tekstu "Lista: [1,2,3]". Dzięki tej funkcjonalności można łatwiej analizować struktury danych i znaleźć ewentualne błędy.

## Zobacz także

- [Debugowanie w języku Haskell](https://medium.com/@yfujii01/debugging-haskell-code-b7ebf47b6673)
- [Funkcja `print` w dokumentacji języka Haskell](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:print)
- [Tutorial o funkcji `show`](https://www.haskell.org/tutorial/numbers.html)