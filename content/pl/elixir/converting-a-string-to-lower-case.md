---
title:    "Elixir: Konwertowanie ciągu znaków na małe litery"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w programowaniu musimy operować na długich ciągach znaków. Aby ułatwić sobie pracę i zapewnić spójność danych, warto umieć konwertować stringi na małe litery.

## Jak to zrobić

W języku Elixir mamy kilka prostych sposobów na konwersję stringów do małych liter. Jedną z nich jest użycie funkcji `String.downcase/1`:

```Elixir
string = "Elixir jest niesamowitym językiem!"
String.downcase(string)
```
```
"elixir jest niesamowitym językiem!"
```

Możemy również użyć konstrukcji `|>` w celu łańcuchowego wywołania funkcji:

```Elixir
"Elixir jest niesamowitym językiem!" |> String.downcase
```
```
"elixir jest niesamowitym językiem!"
```

Warto również zwrócić uwagę na funkcję `String.downcase/2`, która pozwala ustawić język, w którym zostanie wykonana konwersja. Dzięki temu możemy zapewnić poprawną konwersję dla różnych alfabetów.

## Głębszy zanurzenie

Aby lepiej zrozumieć proces konwersji stringów do małych liter w Elixirze, warto poznać kilka szczegółów. Na początek, funkcja `String.downcase/1` wykorzystuje mechanizm "case mapping", który w łatwy sposób pozwala na przekształcenie liter do odpowiednich małych lub wielkich odpowiedników. W przypadku alfabetów spoza standardowych znaków ASCII, kodowanie jest ustalane za pomocą konfiguracji `String.Unicode`.

Ponadto, warto wiedzieć, że funkcja `String.downcase/1` używa funkcji `String.to_charlist/1`, która przekształca ciąg znaków na listę znaków (charlist). Dzięki temu możemy operować na pojedynczych znakach zamiast całego stringa.

## Zobacz również

- Dokumentacja Elixira na temat `String.downcase/1`
- Przewodnik po konwersji stringów i znaków w Elixirze na blogu Politechniki Warszawskiej