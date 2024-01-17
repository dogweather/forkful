---
title:                "Interpolowanie ciągu znaków"
html_title:           "Gleam: Interpolowanie ciągu znaków"
simple_title:         "Interpolowanie ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Interpolacja ciągów znaków jest techniką, która umożliwia wstawienie wartości zmiennych do tekstu w celu łatwiejszego tworzenia i zarządzania danymi. Programiści często używają jej do tworzenia dynamicznych komunikatów lub zapytań.

## Jak to zrobić:

Aby użyć interpolacji w Gleam, możesz oczywiście użyć operatora ```{}``` i wprowadzić odpowiednie zmienne. Na przykład:

```gleam
let imie = "Anna"
let powitaj = "Witaj, {imie}"
```
Wywołanie funkcji ```powitaj``` zwróci tekst "Witaj, Anna".

Możesz również dodać formatowanie do interpolowanego ciągu znaków, na przykład dla liczby:

```gleam
let liczba = 123
let wiadomosc = "Wpłaciłeś {liczba:0}"
```
W tym przypadku wartość zmiennej ```liczba``` będzie wstawiona do tekstu, a następnie sformatowana jako liczba całkowita z zerami na początku, co da nam na wyjściu "Wpłaciłeś 0123".

## Głębszy Zanurzenie:

Interpolacja ciągów znaków jest często używana w językach programowania, ponieważ ułatwia tworzenie czytelnych i dynamicznych komunikatów. Alternatywą dla interpolacji jest konkatenacja (łączenie) ciągów znaków, jednak jest to często nieporęczne i trudniejsze do zarządzania.

W Gleam, tekst zostaje przekształcony w tokeny, które są wstawiane do interpolowanego ciągu, a następnie składane w jeden ciąg znaków. Dzięki temu, interpolacja jest szybsza i zmniejsza ryzyko błędów w porównaniu z konkatenacją.

## Zobacz też:

Więcej informacji o interpolacji ciągów znaków w Gleam znajdziesz w oficjalnej dokumentacji: https://gleam.run/book/standard-library.html#string

Inne przydatne źródła:

- https://en.wikipedia.org/wiki/String_interpolation
- https://docs.python.org/3/library/string.html#formatstrings