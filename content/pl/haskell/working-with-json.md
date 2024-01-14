---
title:                "Haskell: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z formatem JSON jest nieodłączną częścią codziennej pracy programisty w języku Haskell. Jest to popularny sposób przechowywania i przesyłania danych w aplikacjach internetowych, co sprawia, że umiejętność operowania na tych strukturach jest niezbędna. W tej krótkiej notatce dowiesz się, jak łatwo przetwarzać i analizować JSON w Haskell.

## Jak to zrobić?

Przetwarzanie JSON w Haskell jest możliwe dzięki bibliotece `aeson` (od ang. *a JSON parser/printer*). Aby rozpocząć pracę, należy zaimportować odpowiednie moduły:

```Haskell
import Data.Aeson
import Data.ByteString.Lazy
```

Następnie, można stworzyć wartość zawierającą przykładowy plik JSON:

```Haskell
json = "{ \"imie\": \"Jan\", \"nazwisko\": \"Kowalski\", \"wiek\": 25 }"
```

Następnie, dokonujemy konwersji na strukturę danych w języku Haskell:

```Haskell
parsedJSON = decode (pack json) :: Maybe Value
```

Możemy teraz w prosty sposób uzyskać dostęp do konkretnych pól naszego obiektu JSON:

```Haskell
imie = parsedJSON >>= (\o -> o .: "imie") :: Maybe String
```
oraz wyświetlić je na ekranie:

```Haskell
print imie
```

Przykładowy output:

`Just "Jan"`

## Deep Dive

Warto zwrócić uwagę na to, że funkcja `decode` zwraca wartość typu `Maybe`, co oznacza, że może zwrócić w zależności od sytuacji albo poprawną strukturę danych albo `Nothing` (gdy format jest niepoprawny). Dlatego też, warto dokładnie przetestować kod, aby uniknąć nieoczekiwanych błędów.

Kolejną przydatną funkcją z biblioteki `aeson` jest `encode`, która pozwala na konwersję wartości typu `Value` z powrotem do formatu JSON. Jest to szczególnie przydatne, gdy pracujemy z danymi w formacie JSON wewnątrz naszej aplikacji i chcemy przekonwertować je do postaci zrozumiałej dla innych serwisów czy API.

W skrócie, biblioteka `aeson` pozwala na wygodne i łatwe przetwarzanie oraz konwersję danych w formacie JSON w języku Haskell.

## Zobacz także

- [Dokumentacja biblioteki `aeson`](https://hackage.haskell.org/package/aeson)
- [Poradnik dla początkujących do pracy z JSON w Haskell](https://www.snoyman.com/blog/2016/10/beginners-guide-to-json-with-haskell)
- [Porównanie różnych bibliotek do pracy z JSON w Haskell](https://wiki.haskell.org/JSON)

Dziękujemy za lekturę i życzymy owocnych przygód z programowaniem w Haskell!