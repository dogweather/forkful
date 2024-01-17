---
title:                "Interpolacja ciągu znaków"
html_title:           "Elixir: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interpolowanie ciągów znaków polega na wstawianiu wartości zmiennych lub wyrażeń do napisów. To bardzo przydatna funkcja, ponieważ pozwala programistom na tworzenie dynamicznych ciągów znaków, których zawartość może zmieniać się w zależności od danych wejściowych.

## Jak to zrobić?
Interpolowanie ciągów znaków w Elixir jest bardzo proste - wystarczy użyć składni ```#{zmienna}```, gdzie ```zmienna``` jest wartością lub wyrażeniem, które chcesz wstawić. Przykładowy kod wyświetli ciąg znaków z zastosowaniem interpolacji:

```Elixir
# Przykładowe zmienne
imie = "Jan"
wiek = 25

# Tworzenie ciągu znaków z użyciem interpolacji
ciag_znakow = "Cześć, jestem #{imie} i mam #{wiek} lat."

# Wyświetlenie wyniku
IO.puts(ciag_znakow)
```

Wynikiem wywołania powyższego kodu będzie:

```
Cześć, jestem Jan i mam 25 lat.
```

## Głębsza analiza
Interpolacja ciągów znaków istnieje już od wielu lat i jest szeroko stosowana w wielu językach programowania. Oferuje ona wygodną i niezawodną metodę tworzenia dynamicznych ciągów znaków. W niektórych językach, interpolacja jest dostępna tylko w formie specjalnych funkcji lub metod, jednak w Elixir jest to natywna część składni, co czyni ją prostszą i wygodniejszą w użyciu.

Alternatywą dla interpolacji jest łączenie ciągów znaków za pomocą operatora „+”. Jednak jest to mniej efektywne i mniej czytelne rozwiązanie, ponieważ wymaga wielokrotnego pisania operatora, a także nie pozwala na wykorzystanie wartości lub wyrażeń wewnątrz ciągu znaków.

## Zobacz również
https://hexdocs.pm/elixir/String.html#interpolation
https://elixir-lang.org/getting-started/basic-types.html#interpolation