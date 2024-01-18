---
title:                "Przetwarzanie daty z tekstu"
html_title:           "Elixir: Przetwarzanie daty z tekstu"
simple_title:         "Przetwarzanie daty z tekstu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co to jest & Dlaczego?

Wybór daty z ciągu znaków to proces, który polega na zamianie danych tekstowych w odpowiedni format daty. Jest to często wymagane, ponieważ wiele operacji na danych wymaga podania daty w określonym formacie. Programiści wybierają datę z ciągu znaków, aby upewnić się, że ich programy mogą obsługiwać różne formaty dat i przetwarzać je zgodnie z oczekiwaniami.

## Jak to zrobić:

```Elixir
date_string = "25 czerwca 2021"
{:ok, date} = Date.from_iso8601(date_string)
IO.puts date
```
**Output:**
> 2021-06-25

W tym przykładzie używamy funkcji ```Date.from_iso8601```, która analizuje ciąg znaków zawierający datę i zwraca ją w formacie obiektu daty. Ważne jest, aby pamiętać, że funkcja ta zwraca tuple z dwoma elementami - ```{:ok, date}```, dlatego używamy wzorca do dopasowania tylko do drugiego elementu (obiektu daty). Można także użyć innych funkcji, takich jak ```Date.from_erl``` lub ```Date.from_naive```, aby przekonwertować datę z różnych formatów.

## Głębsze wgląd:

Historia datowania sięga tysiące lat wstecz, kiedy to ludzie zaczęli używać różnych systemów datowania. Współczesne struktury dat w formacie YYYY-MM-DD są oparte na standardzie ISO 8601, który został wprowadzony w 1988 roku. Istnieją także inne sposoby na formatowanie dat, takie jak format amerykański (MM/DD/YYYY) lub europejski (DD/MM/YYYY). Jednak z powodu różnych formatów dat w różnych regionach, analizowanie dat z ciągu znaków może być trudne i wymaga zastosowania odpowiednich funkcji

## Zobacz również:

- Dokumentacja modułu Date w języku Elixir: https://hexdocs.pm/elixir/Date.html
- Wprowadzenie do formatu daty ISO 8601: https://www.iso.org/iso-8601-date-and-time-format.html
- Inne metody analizowania dat w języku Elixir: https://hexdocs.pm/elixir/Date.html#functions