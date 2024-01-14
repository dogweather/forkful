---
title:                "Haskell: Zamiana ciągu znaków na małe litery"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie tekstu na małe litery jest częstym zadaniem, które pojawi się podczas programowania w Haskellu. Często jest to wymagane w celu ułatwienia porównywania tekstu lub w celu przetwarzania danych. Jest to również ważne w przypadku, gdy przetwarzanie tekstu będzie odbywać się w środowisku zależnym od wielkości liter, na przykład w systemach operacyjnych.

## Jak to zrobić

Aby zamienić ciąg znaków na małe litery w Haskellu, używamy funkcji `toLower` z modułu `Data.Char`. Spowoduje to zamianę wszystkich dużych liter w tekście na odpowiadające im małe litery.

```Haskell
import Data.Char (toLower)

toLowerCase :: String -> String
toLowerCase text = map toLower text

toLowerCase "HELLO WORLD" --> "hello world"
toLowerCase "HeLlO" --> "hello"
```

W powyższym przykładzie definiujemy funkcję `toLowerCase`, która jako argument przyjmuje ciąg znaków i zwraca zmodyfikowany ciąg, w którym wszystkie litery są małymi literami. Wykorzystujemy tutaj funkcję `map` do aplikacji funkcji `toLower` na każdym elemencie ciągu `text`.

Możemy również wywołać funkcję `toLower` bezpośrednio na pojedynczym znaku.

```Haskell
toLower 'A' --> 'a'
toLower 'b' --> 'b'
```

## Głębszy zanurzenie

Warto zauważyć, że funkcja `toLower` nie zmieni znaków, które nie są literami. Zostaną one po prostu zwrócone w niezmienionej formie.

```Haskell
toLowerCase "123ABC" --> "123abc"
```

Dodatkowo, jeśli chcemy dokonać konwersji tylko na wybranym fragmencie tekstu, możemy wykorzystać wyrażenia lambda.

```Haskell
import Data.Char (toLower)

toLowerCase' :: String -> String
toLowerCase' text = map (\x -> if x `elem` ['A'..'Z'] then toLower x else x) text

toLowerCase' "123ABC" --> "123abc"
toLowerCase' "HeLlO" --> "hello"
```

Funkcja `toLower'` działa bardzo podobnie do funkcji `toLowerCase`, z tą różnicą, że używamy tutaj wyrażenia lambda, aby sprawdzić, czy dany znak jest literą i tylko wtedy zastosować funkcję `toLower`.

## Zobacz także

* [Moduł Data.Char w dokumentacji Haskella](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
* [Funkcja toLower w dokumentacji Haskella](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html#v:toLower)