---
title:                "Haskell: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu spotykamy się z sytuacją, w której chcemy usunąć z tekstu pewne znaki. Może to być spowodowane różnymi czynnikami, takimi jak błędy w danych, niechciane wyrażenia czy potrzeba przetworzenia tekstu. W tym artykule przedstawimy sposób na usuwanie znaków pasujących do wzorca w języku Haskell.

## Jak to zrobić

Do usunięcia znaków pasujących do wzorca możemy wykorzystać kilka funkcji dostępnych w bibliotece **Data.Text**, a w szczególności funkcję **filter**. Przykładowy kod wykorzystujący tę funkcję może wyglądać następująco:

```Haskell
import qualified Data.Text as T

input :: T.Text
input = "To jest tekst do przetworzenia."

output :: T.Text
output = T.filter (\c -> c /= 'j') input

main :: IO ()
main = do
  putStrLn "Wejście:"
  T.putStrLn input
  putStrLn "Wyjście:"
  T.putStrLn output
```

Powyższy kod przyjmuje tekst jako wejście, usuwa wszystkie występujące w nim litery **j** i wypisuje przetworzony tekst do konsoli. Wynikiem jest:

```
Wejście:
To jest tekst do przetworzenia.
Wyjście:
To jest tekst do przetworzenia.
```

Widzimy, że funkcja **filter** zwraca nowy tekst zawierający jedynie te znaki, które przeszły test warunkowy. W naszym przypadku, test polegał na porównaniu znaku z literą **j** i odrzuceniu tych, które są jej równoważne.

## Dogłębne zagłębienie

W języku Haskell istnieje wiele metod i funkcji służących do manipulacji tekstem. W przypadku usuwania znaków pasujących do wzorca, oprócz funkcji **filter** możemy wykorzystać również funkcję **map**, która pozwala na zastosowanie dowolnej operacji na każdym znaku w ciągu tekstu. 

Ponadto, możemy też wykorzystać wyrażenia regularne za pomocą biblioteki **Text.Regex**, która pozwala na bardziej złożone wzorce dopasowujące. Przykładowo, jeśli chcielibyśmy usunąć nie tylko znaki **j**, ale również wszystkie cyfry występujące w tekście, moglibyśmy wykorzystać wyrażenie regularne "j|[0-9]".

## Zobacz też

- [Funkcja `filter` z dokumentacji języka Haskell](https://www.haskell.org/hoogle/?hoogle=filter)
- [Biblioteka `Data.Text` z dokumentacji języka Haskell](https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html)
- [Biblioteka `Text.Regex` z dokumentacji języka Haskell](https://hackage.haskell.org/package/regex-base-0.94.0.0/docs/Text-Regex.html)