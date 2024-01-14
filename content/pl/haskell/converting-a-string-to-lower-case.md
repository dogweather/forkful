---
title:                "Haskell: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w naszych programach musimy zmienić wielkość liter w danym ciągu znaków. Może to być wymagane, na przykład, przy porównywaniu tekstu lub formatowaniu wyświetlania. W tym artykule dowiesz się, dlaczego i jak w łatwy sposób przekonwertować string na małe litery w języku Haskell.

## Jak to zrobić

Aby wykonać tę konwersję, musimy skorzystać z funkcji `map` oraz `toLower` z modułu `Data.Char`. Sprawdźmy to na przykładzie:

```Haskell
import Data.Char (toLower)

convertToLower:: String -> String
convertToLower str = map toLower str
```
W powyższym kodzie najpierw importujemy funkcję `toLower`, a następnie definiujemy funkcję `convertToLower`, która jako argument przyjmuje ciąg znaków. W ciele funkcji wykorzystujemy funkcję `map` do konwersji poszczególnych znaków na małe litery. I gotowe! Teraz wystarczy tylko wywołać funkcję `convertToLower` z wybranym przez nas stringiem.

Przyjrzyjmy się, jak działa ta konwersja na przykładzie:

```Haskell
convertToLower "HELLO WORLD" 
```

Wynikiem będzie:

```
"hello world"
```

Jak widzimy, wszystkie litery zostały zmienione na małe.

## Deep Dive

W języku Haskell istnieje wiele różnych funkcji do manipulowania i przetwarzania ciągów znaków. Jednak użycie funkcji `map` w połączeniu z `toLower` jest jednym z najprostszych i najbardziej wydajnych sposobów na zmianę wielkości liter.

Funkcja `map` działa na listach, a ponieważ napisy w Haskellu są po prostu listami znaków, możemy wykorzystać tę funkcję do przetwarzania ciągów znaków. Funkcja `toLower` z kolei zamienia pojedynczy znak na jego małą wersję. Dzięki temu, łącząc obie funkcje, możemy dokonać konwersji na każdym znaku w podanym napisie.

Sama konwersja na małe litery jest również przydatna przy porównywaniu napisów. Często różne wielkości liter mogą sprawiać problemy w procesie porównywania, dlatego warto wcześniej przekonwertować je na jeden format.

## Zobacz również

- [Oficjalna dokumentacja funkcji map](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:map)
- [Oficjalna dokumentacja funkcji toLower](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toLower)
- [Inne sposoby na manipulację ciągami znaków w Haskellu](https://wiki.haskell.org/String_processing)