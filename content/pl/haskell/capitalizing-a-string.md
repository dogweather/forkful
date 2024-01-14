---
title:    "Haskell: Zapisywanie ciągu wielkimi literami"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego?

Często w programowaniu spotykamy się z potrzebą zmiany wielkości liter w tekście. Czasami jest to kwestia estetyczna, innym razem ma to związek z algorytmami przetwarzania danych. Warto więc poznać jak można w łatwy i szybki sposób zmienić wielkość liter w ciągu znaków w języku Haskell.

## Jak to zrobić?

Do zmiany wielkości liter w Haskellu użyjemy funkcji `toUpper` z modułu `Data.Char`. Poniżej przedstawiony jest przykład użycia tej funkcji w prostym programie, który pobiera tekst od użytkownika i wyświetla go z zamienionymi na wielkie litery.

```Haskell
import Data.Char (toUpper)

main = do
    putStrLn "Wprowadź tekst, który chcesz zamienić na wielkie litery:"
    userInput <- getLine
    putStrLn $ "Twój tekst z wielkimi literami: " ++ map toUpper userInput
```

### Przykładowy output:

```
Wprowadź tekst, który chcesz zamienić na wielkie litery:
Hello World
Twój tekst z wielkimi literami: HELLO WORLD
```

## Głębsze zagłębienie

Funkcja `toUpper` jest dostępna w module `Data.Char` ponieważ jest częścią biblioteki standardowej języka Haskell. Wewnątrz tej funkcji wykorzystywana jest tabela ASCII, która zawiera informacje o kodach znaków, w tym informacje o odpowiedniku wielkiej litery dla każdej małej litery. Dzięki temu możemy w prosty sposób zamienić literę na jej odpowiednik w drugim przypadku.

Należy także zwrócić uwagę, że funkcja `map` w przykładzie powyżej wykorzystuje wyrażenie lambda, czyli funkcję anonimową, aby przetworzyć każdy znak z podanego tekstu. Ten sposób użycia funkcji `map` jest bardzo często stosowany w programowaniu funkcyjnym.

## Zobacz także

* [Dokumentacja modułu `Data.Char` w języku Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
* [Tutorial o funkcjach w języku Haskell](https://www.haskell.org/tutorial/functions.html)
* [ASCII Table - tabela kodów ASCII](http://www.asciitable.com/)