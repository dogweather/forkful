---
title:    "Haskell: Pisanie do standardowego wyjścia błędu"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest ważną częścią programowania w Haskellu. Pozwala na wyświetlanie błędów lub ostrzeżeń w trakcie działania programu, co ułatwia jego debugowanie. W tym artykule przyjrzymy się temu, dlaczego warto używać tej funkcjonalności i jak to zrobić.

## Jak to zrobić

Aby napisać do standardowego błędu w Haskellu, możemy użyć funkcji `hPutStrLn` z modułu `System.IO`. Przykładowy kod wyglądałby następująco:

```Haskell
import System.IO

main :: IO ()
main = do
    hPutStrLn stderr "To jest przykładowy błąd"
    putStrLn "Kod zostanie wyświetlony po błędzie"
```

Wywołanie `hPutStrLn stderr` pozwala na wyświetlenie wiadomości do standardowego błędu, a następnie kod programu może kontynuować swoje działanie. W ten sposób można wyświetlać błędy, ostrzeżenia lub informacje dla użytkownika w trakcie działania programu.

## Głębszy wykład

Dodatkowo, w celu zwiększenia czytelności wiadomości wyświetlanych do standardowego błędu, można użyć modułu `Text.Printf` i funkcji `hPrintf`. Pozwala to na formatowanie wiadomości, np. wstawianie wartości zmiennych, co może być szczególnie przydatne przy wyświetlaniu informacji lub błędów związanych z konkretnymi danymi. Przykładowy kod wyglądałby tak:

```Haskell
import System.IO
import Text.Printf

main :: IO ()
main = do
    let liczba = 5
    hPrintf stderr "Liczba %d jest mniejsza niż 10" liczba
    putStrLn "Kod zostanie wyświetlony po błędzie"
```

W tym przypadku, w miejsce `%d` zostanie wstawiona wartość zmiennej `liczba`, co da efekt "Liczba 5 jest mniejsza niż 10".

## Zobacz też

- [Dokumentacja modułu System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Dokumentacja modułu Text.Printf](https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-Printf.html)
- [Sekcja "I/O and Interacting with the World" w książce "Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/input-and-output)