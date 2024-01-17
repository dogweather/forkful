---
title:                "Porównywanie dwóch dat"
html_title:           "Elixir: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Czym jest porównywanie dwóch dat i dlaczego programiści to robią?
Porównywanie dwóch dat to proces porównywania dwóch punktów w czasie, aby określić, która data jest wcześniejsza lub późniejsza. Jest to przydatne w wielu przypadkach, na przykład w systemach rezerwacji, raportów finansowych lub planowania zadań.

## Jak to zrobić:
```Elixir
# Importowanie modułu Date
import Date

# Definiowanie dwóch dat
date1 = Date.new!(2021, 3, 15)
date2 = Date.new!(2021, 4, 1)

# Porównywanie dat
case Date.compare(date1, date2) do
  :lt ->
    IO.puts("Data 1 jest wcześniejsza niż data 2")
  :gt ->
    IO.puts("Data 2 jest wcześniejsza niż data 1")
  :eq ->
    IO.puts("Obie daty są sobie równe")
end
```
Output:
```
Data 1 jest wcześniejsza niż data 2
```

## Wnikliwe spojrzenie:
Porównywanie dat jest możliwe dzięki temu, że Elixir posiada moduł Date, który udostępnia funkcję compare do porównania dwóch dat. Alternatywnie, można użyć operatora `==` lub funkcji Date.diff, aby porównać daty.

## Zobacz również:
Oficjalny podręcznik Elixira: https://hexdocs.pm/elixir/Date.html#compare/2