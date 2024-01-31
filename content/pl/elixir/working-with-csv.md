---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z plikami CSV (Comma-Separated Values - wartości oddzielone przecinkiem) to codzienność programistów. Używamy ich, bo to prosty format wymiany danych, którego można używać między różnymi systemami i językami programowania.

## Jak to zrobić:
Elixir używa pakietów, jak `CSV`, do obsługi plików CSV. Zobacz przykłady:

```elixir
# 1. Dodaj pakiet CSV do twojego mix.exs
defp deps do
  [
    {:csv, "~> 2.4"}
  ]
end

# 2. Odczytaj plik CSV
{:ok, pid} = CSV.decode_file("twoj_plik.csv")
for row <- CSV.decode_stream(pid), do: IO.inspect(row)

# Przykładowe dane:
# imie,nazwisko,wiek
# Jan,Kowalski,30
# Anna,Nowak,25

# Przykładowy wynik:
# ["Jan", "Kowalski", "30"]
# ["Anna", "Nowak", "25"]
```

## Głębsze spojrzenie:
CSV jest stary jak świat - powstał w 1972 roku! Dzisiaj są alternatywy jak JSON czy XML, ale CSV świetnie się sprawdza do tabelarycznych danych. Elixir używa strumieni i wzorców do efektywnej pracy z CSV - to oszczędza pamięć, szczególnie przy dużych plikach.

## Zobacz też:
- Dokumentacja pakietu CSV: [https://hexdocs.pm/csv/readme.html](https://hexdocs.pm/csv/readme.html)
- Artykuł "Elixir i CSV: Pierwsze kroki": [https://medium.com/@julien__le_coupanec/processing-csv-with-elixir-1bbd3865dd73](https://medium.com/@julien__le_coupanec/processing-csv-with-elixir-1bbd3865dd73)
