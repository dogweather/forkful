---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Elm: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy, kto programuje w Elm, może od czasu do czasu potrzebować skonwertować datę na ciąg znaków, na przykład aby wyświetlić ją w czytelnej formie dla użytkownika. W tym artykule opowiem o tym, jak dokonać tego w prosty i efektywny sposób.

## Jak to zrobić

Aby skonwertować datę na ciąg znaków w Elm, potrzebujemy użyć funkcji `toString` z biblioteki `Date`. Poniżej znajduje się przykładowy kod, który wykorzystuje tę funkcję oraz wyświetla wynik w konsoli:

 ```Elm
 import Date exposing (Day, Month, Year, toIsoString)

 dateToString : Date -> String
 dateToString date =
  toIsoString date

 date : Date
 date =
  Date.fromYearMonthDay 2020 10 27

 main : Program () String Never
 main =
  dateToString date
    |> Task.succeed
 ```

Przykładowy wynik w konsoli będzie wyglądał następująco: `"2020-10-27"`. W tym przykładzie wykorzystaliśmy funkcje `fromYearMonthDay` z biblioteki `Date` do stworzenia daty, a następnie przekazaliśmy ją do funkcji `dateToString`, która zwraca nam ciąg znaków w formacie ISO.

## Pogłębiony wykład

Jeśli chcemy konwertować datę na ciąg znaków w innym formacie niż ISO, musimy użyć funkcji `format` z biblioteki `Date`. W przykładzie poniżej użyjemy tej funkcji do zmiany formatu daty na DD/MM/YYYY:

 ```Elm
 import Date exposing (Day, Month, Year, format)

 dateToString : Date -> String
 dateToString date =
  format "%d/%m/%Y" date

 date : Date
 date =
  Date.fromYearMonthDay 2020 10 27

 main : Program () String Never
 main =
  dateToString date
    |> Task.succeed
 ```

Przykładowy wynik w konsoli to `"27/10/2020"`. Możemy także zmieniać format daty w bardziej skomplikowany sposób, np. dodając godzinę i minuty. Możliwości są praktycznie nieograniczone!

## Zobacz także

- Dokumentacja biblioteki `Date` w Elm: https://package.elm-lang.org/packages/elm/time/latest/Date
- Inne sposoby konwertowania daty na ciąg znaków w Elm: https://stackoverflow.com/questions/46428149/elm-format-date-string